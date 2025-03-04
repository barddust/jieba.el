#!/bin/sh

mkdir -p ~/.emacs.d/jieba/
cd ~/.emacs.d/jieba/

VENV_NAME=~/.emacs.d/jieba/jieba-venv
SERVER_SCRIPT=~/.emacs.d/elpa/jieba/jieba-server.py

if ! command -v python &>/dev/null; then
    echo "Python 未安装，正在安装..."
    sudo pacman -Syu --noconfirm python
else
    echo "Python 已安装，版本信息："
    python --version
fi

python -m venv "$VENV_NAME"
source $VENV_NAME/bin/activate

$VENV_NAME/bin/python -m pip install jieba websockets jsonrpcserver

# 创建 systemd 用户服务目录（如果不存在）
mkdir -p ~/.config/systemd/user

# 定义 systemd 服务文件路径
SERVICE_FILE=~/.config/systemd/user/jieba-python.service

# 写入 systemd 服务文件
cat > "$SERVICE_FILE" <<EOF
[Unit]
Description=Run Python Jieba Server
After=network.target

[Service]
ExecStart=$VENV_NAME/bin/python $SERVER_SCRIPT
Restart=on-failure

[Install]
WantedBy=default.target
EOF

echo "Systemd 服务文件已创建：$SERVICE_FILE"

# 重新加载 systemd 用户服务
systemctl --user daemon-reload

# 启用并启动服务
systemctl --user enable jieba-python.service
systemctl --user start jieba-python.service
