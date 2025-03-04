import asyncio
from websockets.asyncio.server import serve
from jsonrpcserver import method, Success, Error, async_dispatch
import json
import jieba

address = "127.0.0.1"
port = 58291
cache_file = None

@method
async def hello(cache_file: str = None):
    if cache_file:
        jieba.dt.cache_file = cache_file
    if not jieba.dt.initialized:
        jieba.initialize()
    return Success("[JIEBA] Jieba is initialized")

@method
async def loadDict(dicts: dict):
    import traceback
    success = failed = 0
    for dict in dicts:
        try:
            jieba.load_userdict(dict)
            success += 1
        except:
            print(traceback.format_exc())
            failed += 1
    return Success("[JIEBA] Try to load %d Dict(s), %d successed, %d failed." % (len(dicts), success, failed))

@method
async def split(string: str):
    print("Splitting: %s" % string)
    res = jieba.lcut(string)
    return Success(res)

async def jsonrpc(websocket):
    try:
        async for msg in websocket:
            await websocket.send(await async_dispatch(msg))
    except:
        pass

async def main():
    async with serve(jsonrpc, address, port) as server:
        print("Jieba Server started")
        await server.serve_forever()

if __name__ == '__main__':
    asyncio.run(main())
