import asyncio
import time
import threading
import queue


q1 = queue.Queue()

def system_thread():
    for i in range(1000):
        time.sleep(2)
        q1.put('obj-%d'%i)
    return 0

threading.Thread(target = system_thread).start()

async def task1():
    for i in range(1000):
        await asyncio.sleep(1)
        print('task1 is alive', i)
    return 0

async def main():
    loop = asyncio.get_running_loop()
    asyncio.create_task(task1())
    while 1:
        result = await loop.run_in_executor(None, lambda: q1.get())
        print('get', result)
    return 0

asyncio.run(main())

'''
task1 is alive 0
get obj-0
task1 is alive 1
task1 is alive 2
get obj-1
task1 is alive 3
task1 is alive 4
get obj-2
task1 is alive 5
task1 is alive 6
get obj-3
task1 is alive 7
task1 is alive 8
'''


