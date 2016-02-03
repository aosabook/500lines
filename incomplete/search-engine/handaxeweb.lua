#!/usr/bin/lua
function register_chunk(chunks, new_chunk)
    if new_chunk.name == nil then return end

    local contents = chunks[new_chunk.name]
    if not contents then 
        contents = {}
        chunks[new_chunk.name] = contents
    end

    -- If there’s a duplicate, append text to it.
    for _, it in ipairs(chunks[new_chunk.name]) do
        if it.v == new_chunk.v then
            for _, line in ipairs(new_chunk.text) do
                table.insert(it.text, line)
            end
            return
        end
    end

    -- No duplicate. Add to table.
    table.insert(contents, new_chunk)
end

function is_indented(line)
    return string.match(line, "^    ")
end

assert(    is_indented("    hi"))
assert(not is_indented("   hi"))
assert(not is_indented("   hi    "))

function unindented(line) return string.sub(line, 5) end
assert(unindented("    hi\n") == "hi\n")

function get_chunk_label(line)
    return string.match(line, "^[^%w]*in (.*):[^%w]*$")
end

assert(get_chunk_label("-- in handaxeweb.lua:") ==
       "handaxeweb.lua")
assert(get_chunk_label("/* in handaxeweb.c: */") ==
       "handaxeweb.c")
assert(get_chunk_label("# in a minute: #\n") ==
       "a minute")

function parse_chunk_label(label)
    local name, version = 
        string.match(label, "(.*) v(%d+)$")
    if name then return name, tonumber(version)
    else return label, 0 end
end

assert(parse_chunk_label("foo") == "foo")
assert(({parse_chunk_label("foo")})[2] == 0)
assert(parse_chunk_label("foo v32") == "foo")
assert(({parse_chunk_label("foo v32")})[2] == 32)

function parse_input()
    local chunks, current_chunk, in_chunk = {}, {text={}}, false
    local blank_lines = {}

    for line in io.lines() do
        if string.match(line, "^%s*$") then -- blank line
            if in_chunk then table.insert(blank_lines, "") end
        elseif not in_chunk and is_indented(line) then
            local label = get_chunk_label(line)
            
            if label then  -- if that succeeded, change chunks
                register_chunk(chunks, current_chunk)
                local name, ver = parse_chunk_label(label)
                current_chunk = {name = name, v = ver, text = {}}
            else
                -- incorporate any blank lines seen in between indented lines
                for _, blank_line in ipairs(blank_lines) do
                    table.insert(current_chunk.text, blank_line)
                end
                blank_lines = {}
                
                table.insert(current_chunk.text, unindented(line))
            end
            in_chunk = true
        elseif in_chunk and is_indented(line) then
            -- incorporate any blank lines seen in between indented lines
            for _, blank_line in ipairs(blank_lines) do
                table.insert(current_chunk.text, blank_line)
            end
            blank_lines = {}
            
            table.insert(current_chunk.text, unindented(line))
        else
            blank_lines = {}
            in_chunk = false
        end
    end
    register_chunk(chunks, current_chunk)

    return chunks
end

function list_chunk_names_and_versions(chunks)
    io.write("# Listing versions and root chunk names.\n")
    io.write("# Version 12 is displayed as:\n")
    io.write("# v 12\n")
    io.write("# Chunk name foo bar is displayed as:\n")
    io.write("# n foo bar\n")
    io.write("# To tangle a particular root chunk, run:\n")
    io.write("# "..arg[0].." chunkname\n")
    io.write("# That tangles version 0 by default; to specify v69:\n")
    io.write("# "..arg[0].." chunkname 69\n")

    local versions, referenced_chunks = {}, {}
    for name, contents in pairs(chunks) do
        for _, it in ipairs(contents) do
            versions[it.v] = true
    
            for _, line in ipairs(it.text) do
                local _, chunkname = parse_reference(line)
                if chunkname ~= nil then 
                    referenced_chunks[chunkname] = true
                end
            end
        end
    end

    for version, _ in pairs(versions) do
        io.write(string.format("v %d\n", version))
    end

    for name, _ in pairs(chunks) do
        if not referenced_chunks[name] then
            io.write("n "..name.."\n")
        end
    end
end

function get_chunk_text(contents, version)
    local best
    for _, it in ipairs(contents) do
        if it.v <= version and (not best or
                                it.v > best.v) then
            best = it
        end
    end
    if best then return best.text else return nil end
end

do
    local contents = {{v=0, text={"a"}},
                      {v=2, text={"b"}},
                      {v=1, text={"c"}}}
    assert(get_chunk_text(contents, 0)[1] == "a")
    assert(get_chunk_text(contents, 1)[1] == "c")
    assert(get_chunk_text(contents, 2)[1] == "b")
    assert(get_chunk_text(contents, 3)[1] == "b")
    assert(get_chunk_text(contents, -1) == nil)
end

function parse_reference(line)
    return string.match(line, "^(%s*)<<(.*)>>(%s*)$")
end

do
    local indent, name = parse_reference("  <<foo>>\n")
    assert(indent == "  ")
    assert(name == "foo")
    assert(parse_reference("bits << shiftlen >> 1") == nil)
end

function tangle(chunks, chunkname, version, indent)
    if indent == nil then indent = '' end

    local contents = chunks[chunkname]
    if contents == nil then
        error(string.format("chunk `%s` does not exist", 
                            chunkname))
    end
    
    local text = get_chunk_text(contents, version)
    if text == nil then 
        error(string.format("chunk `%s` has no version `%d`",
                            chunkname, version))
    end

    for _, line in ipairs(text) do
        local nindent, nchunkname = parse_reference(line)
        if nindent then
            tangle(chunks, nchunkname, version, indent..nindent)
        else
            io.write(indent..line.."\n")
        end
    end
end

local chunks = parse_input()
chunkname, version = ...
if chunkname == nil then
    list_chunk_names_and_versions(chunks)
else
    if version == nil then version = 0 end
    tangle(chunks, chunkname, tonumber(version))
end
