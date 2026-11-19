#!/bin/sh
# Stands in for curl talking to an MCP server with one tool, "search".
# Arguments, configuration and requests are recorded next to the script.
# A copy whose name contains "reject" refuses every request.
config=$(cat)
echo "$@" >> "$0.args"
printf '%s\n' "$config" >> "$0.stdin"
body=$(printf '%s\n' "$config" | sed -n 's/^data-binary = "@\(.*\)"$/\1/p')
headers=$(printf '%s\n' "$config" | sed -n 's/^dump-header = "\(.*\)"$/\1/p')
request=$(cat "$body")
printf '%s\n' "$request" >> "$0.requests"
status='200 OK'
case "$0" in
  *reject*)
    printf 'HTTP/1.1 403 Forbidden\r\n\r\n' > "$headers"
    echo '{"error":"this needs the mcp capability"}'
    exit 0 ;;
esac
case "$request" in
  *'"method":"initialize"'*)
    reply='{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-03-26","capabilities":{}}}' ;;
  *notifications/initialized*)
    status='202 Accepted'
    reply='' ;;
  *tools/list*)
    reply='event: message
data: {"jsonrpc":"2.0","method":"notifications/message","params":{}}

event: message
data: {"jsonrpc":"2.0","id":2,"result":{"tools":[{"name":"search","description":"Search the wiki.","inputSchema":{"type":"object","properties":{"query":{"type":"string"}}}}]}}
' ;;
  *'"name":"search"'*)
    reply='{"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"Page one"},{"type":"image","data":""},{"type":"text","text":"Page two"}],"isError":false}}' ;;
  *)
    reply='{"jsonrpc":"2.0","id":4,"error":{"code":-32602,"message":"Unknown tool"}}' ;;
esac
printf 'HTTP/1.1 %s\r\nMcp-Session-Id: session-7\r\n\r\n' "$status" > "$headers"
printf '%s\n' "$reply"
