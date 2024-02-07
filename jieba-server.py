import asyncio
import jieba
import json

address = '127.0.0.1'
port = 58291

class JiebaServer:
    errors = {
        -32700: "Parse error",
        -32600: "Invalid Request",
        -32601: "Method not found",
        -32602: "Invalid params",
        -32603: "Internal error",
    }
    
    def hello(self, *args):
        print("Jieba Initializing")
        jieba.initialize()
        return "[JIEBA] Connection created."

    def split(self, string: str):
        print("Splitting: %s" % string)
        res = jieba.lcut(string)
        return res

    def loadDict(self, dicts: dict):
        import traceback
        success = failed = 0
        for dict in dicts: 
            try:
                jieba.load_userdict(dict)    
                success += 1
            except:
                print(traceback.format_exc())
                failed += 1
        return "[JIEBA] Try to load %d Dict(s), %d successed, %d failed." % (len(dicts), success, failed)

    async def _send_result(self, writer, thing, id=None):
        res = {
            "jsonrpc": "2.0",
            "result": thing,
            "id": id
        }
        writer.write(json.dumps(res).encode())
        await writer.drain()

    async def _send_error(self, writer, code, id=None):
        res = {
            "jsonrpc": "2.0",
            "error": {"code": code, "message": self._get_error_message(code)},
            "id": id
        }
        writer.write(json.dumps(res).encode())
        await writer.drain()

    def _get_error_message(self, code):
        return self.errors.get(code, "Server error")

    
    async def handler(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        # The first loop is for re-use connection
        # the second loop is Emacs RPC won't send EOF.
        while True:
            res = b""
            data = b""
            brace_num = 0
            while True:
                chunk = await reader.read(1)
                data += chunk
                if chunk == b"{":
                    brace_num += 1
                elif chunk == b"}":
                    brace_num -= 1

                if not brace_num:
                    break
            if not data:
                break

            json_data = json.loads(data)
            print("Received :%s" % json_data)

            id = json_data.get("id", None)
            try:
                method = json_data["method"]
                fn = getattr(self, method)
            except:
                await self._send_error(writer, -32601, id)
                continue

            await self._send_result(
                writer,
                fn(json_data.get("params", None)),
                id
            )
            
        writer.close()
        await writer.wait_closed()

    async def run(self, address, port):
        server = await asyncio.start_server(
            self.handler, address, port)
        print("Serving at %s:%d" % (address , port))
        async with server:
            await server.serve_forever()

if __name__ == "__main__":
    server = JiebaServer()
    asyncio.run(server.run(address, port))

