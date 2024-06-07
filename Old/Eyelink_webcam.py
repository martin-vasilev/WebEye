# -*- coding: utf-8 -*-
"""
Created on Thu May 30 15:31:28 2024

@author: Martin
"""

import nest_asyncio
nest_asyncio.apply()
import asyncio
import websockets
import json


async def websocket_server():
    while True:
        websockets.serve(server_handler, "localhost", 8081)
        print("Doing the snmp thing")
        await asyncio.sleep(1)

async def proxy():
    while True:
        print("Doing the proxy thing")
        await asyncio.sleep(2)

async def main():
        tasks = []
        tasks.append(asyncio.create_task(websocket_server()))
        tasks.append(asyncio.create_task(proxy()))

        await asyncio.gather(*tasks)

asyncio.run(main())