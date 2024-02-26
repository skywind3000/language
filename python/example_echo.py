import asyncio

async def echo_client():
    reader, writer = await asyncio.open_connection('127.0.0.1', 5001)
    while 1:
        writer.write(b'HELLO')
        await writer.drain()
        p = await reader.readexactly(5)
        if p == b'HELLO':
            print('ping')
        else:
            print('error')
        await asyncio.sleep(1)
    return 0

async def echo_server(reader, writer):
    while True:
        s = await reader.read(4096)
        if not s:
            break
        writer.write(s)
        await writer.drain()
    return 0


async def main():
    server = await asyncio.start_server(echo_server, '0.0.0.0', 5001)
    task = asyncio.create_task(echo_client())
    print('serving on {}'.format(server.sockets[0].getsockname()))
    await server.serve_forever()
    task.cancel()
    return 0


asyncio.run(main())


