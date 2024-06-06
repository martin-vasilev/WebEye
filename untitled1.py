# -*- coding: utf-8 -*-
"""
Created on Tue Jun  4 00:19:39 2024

@author: Martin
"""
import nest_asyncio
import asyncio
import websockets
import json

# Apply the nest_asyncio patch
nest_asyncio.apply()

async def communicate(uri):
    async with websockets.connect(uri) as websocket:
        while True:
            # Collect message and additional data
            message = input("Enter a message to send: ")
            additional_data = {
                "username": "user1",
                "timestamp": "2024-06-04T12:34:56Z"
            }

            # Combine message and additional data into a single dictionary
            data_to_send = {
                "msg": 'el_x',
                "data": 500
            }

            # Convert the dictionary to a JSON string
            json_message = json.dumps(data_to_send)

            # Send the JSON string over the WebSocket
            await websocket.send(json_message)
            print(f"Sent: {json_message}")

            # Receive a response from the WebSocket server
            response = await websocket.recv()
            print(f"Received: {response}")

uri = "ws://localhost:8081"  # Replace with your WebSocket server URI

asyncio.get_event_loop().run_until_complete(communicate(uri))

