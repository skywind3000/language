import asyncio
import time

async def task1(n):
    print('<task-%d>'%n)
    await asyncio.sleep(1)
    print('</task-%d>'%n)
    return n * 10

async def main():
    for i in range(10):
        p = asyncio.create_task(task1(i))
    while 1:
        await asyncio.sleep(1)
    return 0

asyncio.run(main())

