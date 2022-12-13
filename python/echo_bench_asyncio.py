import asyncio
import hiredis

d = {}

def process(req):
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
    elif cmd == b'config':
        return b'-ERROR\r\n'
    else:
        return b'-ERROR\r\n'
    return b''

async def echo_server(reader, writer):
    hireader = hiredis.Reader()
    while True:
        s = await reader.read(4096)
        if not s:
            break
        # print('received %d bytes'%len(s))
        # print(s.decode('latin1'))
        hireader.feed(s)
        while True:
            req = hireader.gets()
            if not req:
                break
            res = process(req)
            writer.write(res)
            await writer.drain()
    return 0

async def main():
    server = await asyncio.start_server(echo_server, '0.0.0.0', 5001)
    print('serving on {}'.format(server.sockets[0].getsockname()))
    await server.serve_forever()
    return 0

asyncio.run(main())

