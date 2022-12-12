import asyncio

async def handle_echo(reader, writer):
    print("New connection")
    data = await reader.read(100)
    message = data.decode()
    addr = writer.get_extra_info('peername')
    print(f"Received {message!r} from {addr!r}")
    print(f"Send: {message!r}")
    writer.write(data)
    await writer.drain()
    print("Close the connection")
    writer.close()

async def handle_echo2(reader, writer):
    while 1:
        data = await reader.read(100)
        writer.write(data)
        writer.write(b'  ')
        await writer.drain()
        print('received %d bytes'%len(data))
    return 0

async def task1():
    for i in range(100):
        # print("timer: %d"%i)
        await asyncio.sleep(1)
    return 0

def blocking_function():
    import time
    print('<block>')
    for i in range(5):
        print('  block', i)
        time.sleep(1)
    print('</block>')
    return 10

async def task2():
    loop = asyncio.get_running_loop()
    print('<task2>')
    result = await loop.run_in_executor(None, blocking_function)
    print('</task2>')
    print('result', type(result), result)
    return 0

async def main():
    server = await asyncio.start_server(
            handle_echo2, '0.0.0.0', 8888)

    addr = server.sockets[0].getsockname()
    print(f"Serving on {addr}")

    t1 = asyncio.create_task(task1())
    t2 = asyncio.create_task(task1())
    t3 = asyncio.create_task(task2())

    # async with server:
    await server.serve_forever()

    return 0

asyncio.run(main())


