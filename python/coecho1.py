import asyncio

async def handle_echo(reader, writer):
    while 1:
        data = await reader.read(100)
        writer.write(data)
        await writer.drain()
        print('received %d bytes'%len(data))
        if data.strip() == b'exit':
            break
    writer.close()
    return 0


async def main():
    server = await asyncio.start_server(
            handle_echo, '0.0.0.0', 8888)

    addr = server.sockets[0].getsockname()
    print(f"Serving on {addr}")

    # async with server:
    await server.serve_forever()

    return 0

asyncio.run(main())


