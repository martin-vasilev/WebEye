# -*- coding: utf-8 -*-
"""
Created on Thu May 23 13:53:11 2024

@author: Martin
"""


import asyncio
import websockets

async def server_handler(websocket, path):
    try:
        while True:
            #await asyncio.sleep(1)
          #  message = "Hello from server"
           # await websocket.send(message)
            #print(f"Sent message: {message}")
            
            message = await websocket.recv()
            print(f"Received message: {message}")
            
    except websockets.ConnectionClosed:
        print("Client disconnected")

start_server = websockets.serve(server_handler, "localhost", 8081)

asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()
