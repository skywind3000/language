import sys
import gevent
import gevent.monkey
import hiredis

from gevent.server import StreamServer

gevent.monkey.patch_all()

d = {}

def process(req):
    # only support get/set
    cmd = req[0].lower()
    if cmd == b'set':
        d[req[1]] = req[2]
        return b"+OK\r\n"
    elif cmd == b'get':
        v = d.get(req[1])
        if v is None:
            return b'$-1\r\n'
        else:
            return b'$1\r\n1\r\n'
            #return '$%d\r\n%s\r\n' % (len(v), v)
    else:
        print(cmd)
        raise NotImplementedError()
    return b''

def handle(sock, addr):
    reader = hiredis.Reader()
    while True:
        buf = sock.recv(4096)
        if not buf:
            return
        reader.feed(buf)
        while True:
            req = reader.gets()
            if not req:
                break
            sock.sendall(process(req))
    return 0


print('serving on 0.0.0.0:5000')
server = StreamServer(('0.0.0.0', 5000), handle)
server.serve_forever()


