import asyncio

async def foo():
    return 10

async def main():
    coro = foo()
    task = asyncio.create_task(coro)
    done, pending = await asyncio.wait({task})
    if task in done:
        print('done')
    elif task in pending:
        print('pending')
    else:
        print('else')
    return 0

asyncio.run(main())


