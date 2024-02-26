import time
import asyncio
import sys
import random

clients = {}

async def echo_client(uid, remote):
    print('[%d] client starting'%uid)
    reader, writer = None, None
    for i in range(3):
        try:
            reader, writer = await asyncio.open_connection(remote[0], remote[1])
        except Exception as e:
            print('[%d] error: %s'%(uid, str(e)))
            await asyncio.sleep(0.1 + random.random())
            continue
        break
    if reader is None:
        print('[%d] cannot create connection'%uid)
        sys._exit()
    info = {}
    info['uid'] = uid
    info['activate'] = time.time()
    info['rtt'] = -1
    clients[uid] = info
    while 1:
        text = str(time.time()) + '\n'
        writer.write(text.encode('utf-8'))
        await writer.drain()
        data = await reader.readline()
        text = data.decode('utf-8', 'ignore').strip()
        rtt = time.time() - float(text)
        info['rtt'] = rtt
        info['activate'] = time.time()
        if uid == 0:
            print('[%d] rtt: %.03f ms'%(uid, rtt * 1000))
        await asyncio.sleep(1)
    del clients[uid]
    return 0


async def main():
    remote = ('127.0.0.1', 8080)
    count = 5000
    tasks = []
    for i in range(count):
        t = asyncio.create_task(echo_client(i, remote))
        tasks.append(t)
        await asyncio.sleep(0.02)
    while 1:
        await asyncio.sleep(1)
        current = time.time()
        alive = 0
        total = len(clients)
        for uid in clients:
            info = clients[uid]
            activate = current - info['activate']
            if activate <= 2:
                if info['rtt'] < 0.5:
                    alive += 1
        print('stats number is %d/%d'%(alive, total))
    return 0


asyncio.run(main())


