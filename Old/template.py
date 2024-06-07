# -*- coding: utf-8 -*-
"""
Created on Thu May 30 15:29:03 2024

@author: Martin
"""

import nest_asyncio
nest_asyncio.apply()
import asyncio

async def websocket_server():
    while True:
        print("Doing the snmp thing")
        await asyncio.sleep(1)

async def proxy():
    while True:
        print("Doing the proxy thing")
        await asyncio.sleep(2)

async def main():
        tasks = []
        tasks.append(asyncio.create_task(snmp()))
        tasks.append(asyncio.create_task(proxy()))

        await asyncio.gather(*tasks)

asyncio.run(main())