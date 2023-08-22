-- 
--  This is file `luacolor.lua',
--  generated with the docstrip utility.
-- 
--  The original source files were:
-- 
--  luacolor.dtx  (with options: `lua')
--  
--  This is a generated file.
--  
--  Project: luacolor
--  Version: 2023-08-18 v1.18
--  
--  Copyright (C)
--     2007, 2009-2011 Heiko Oberdiek
--     2016-2023 Oberdiek Package Support Group
--  
--  This work may be distributed and/or modified under the
--  conditions of the LaTeX Project Public License, either
--  version 1.3c of this license or (at your option) any later
--  version. This version of this license is in
--     https://www.latex-project.org/lppl/lppl-1-3c.txt
--  and the latest version of this license is in
--     https://www.latex-project.org/lppl.txt
--  and version 1.3 or later is part of all distributions of
--  LaTeX version 2005/12/01 or later.
--  
--  This work has the LPPL maintenance status "maintained".
--  
--  The Current Maintainers of this work are
--  Heiko Oberdiek and the Oberdiek Package Support Group
--  https://github.com/ho-tex/luacolor/issues
--  
--  
--  This work consists of the main source file luacolor.dtx
--  and the derived files
--     luacolor.sty, luacolor.pdf, luacolor.ins, luacolor.drv,
--     luacolor.lua,
--  
oberdiek = oberdiek or {}
local luacolor = oberdiek.luacolor or {}
oberdiek.luacolor = luacolor
function luacolor.getversion()
  tex.write("2023-08-18 v1.18")
end
local ifpdf = tonumber(tex.outputmode or tex.pdfoutput) > 0
local prefix
local prefixes = {
  dvips   = "color ",
  dvipdfm = "pdf:sc ",
  truetex = "textcolor:",
  pctexps = "ps::",
}
local patterns = {
  ["^color "]            = "dvips",
  ["^pdf: *begincolor "] = "dvipdfm",
  ["^pdf: *bcolor "]     = "dvipdfm",
  ["^pdf: *bc "]         = "dvipdfm",
  ["^pdf: *setcolor "]   = "dvipdfm",
  ["^pdf: *scolor "]     = "dvipdfm",
  ["^pdf: *sc "]         = "dvipdfm",
  ["^textcolor:"]        = "truetex",
  ["^ps::"]              = "pctexps",
}
local function info(msg, term)
  local target = "log"
  if term then
    target = "term and log"
  end
  texio.write_nl(target, "Package luacolor info: " .. msg .. ".")
  texio.write_nl(target, "")
end
function luacolor.dvidetect()
  local v = tex.box[0]
  assert(v.id == node.id("hlist"))
  for v in node.traverse_id(node.id("whatsit"), v.head) do
    if v and v.subtype == node.subtype("special") then
      local data = v.data
      for pattern, driver in pairs(patterns) do
        if string.find(data, pattern) then
          prefix = prefixes[driver]
          tex.write(driver)
          return
        end
      end
      info("\\special{" .. data .. "}", true)
      return
    end
  end
  info("Missing \\special", true)
end
local map = {
  n = 0,
}
function luacolor.get(color)
  tex.write("" .. luacolor.getvalue(color))
end
function luacolor.getvalue(color)
  local n = map[color]
  if not n then
    n = map.n + 1
    map.n = n
    map[n] = color
    map[color] = n
  end
  return n
end
local attribute
function luacolor.setattribute(attr)
  attribute = attr
end
function luacolor.getattribute()
  return attribute
end
local LIST = 1
local LIST_LEADERS = 2
local LIST_DISC = 3
local COLOR = 4
local NOCOLOR = 5
local RULE = node.id("rule")
local node_types = {
  [node.id("hlist")] = LIST,
  [node.id("vlist")] = LIST,
  [node.id("rule")]  = COLOR,
  [node.id("glyph")] = COLOR,
  [node.id("disc")]  = LIST_DISC,
  [node.id("whatsit")] = {
    [node.subtype("pdf_colorstack")] =
      function(n)
        return n.stack == 0 and NOCOLOR or nil
      end,
    [node.subtype("special")] = COLOR,
    [node.subtype("pdf_literal")] = COLOR,
    [node.subtype("pdf_save")] = COLOR,
    [node.subtype("pdf_restore")] = COLOR, -- probably not needed
-- TODO (DPC)    [node.subtype("pdf_refximage")] = COLOR,
  },
  [node.id("glue")] =
    function(n)
      if n.subtype >= 100 then -- leaders
        if n.leader.id == RULE then
          return COLOR
        else
          return LIST_LEADERS
        end
      end
    end,
}
local function get_type(n)
  local ret = node_types[n.id]
  if type(ret) == 'table' then
    ret = ret[n.subtype]
  end
  if type(ret) == 'function' then
    ret = ret(n)
  end
  return ret
end
local mode = 2 -- luatex.pdfliteral.direct
local WHATSIT = node.id("whatsit")
local SPECIAL = node.subtype("special")
local PDFLITERAL = node.subtype("pdf_literal")
local DRY_FALSE = false
local DRY_TRUE = true
local function traverse(list, color, dry)
  if not list then
    return color
  end
  local head
  if get_type(list) == LIST then
    head = list.head
  elseif get_type(list) == LIST_DISC then
    head = list.replace
  else
    texio.write_nl("!!! Error: Wrong list type: " .. node.type(list.id))
    return color
  end
  for n in node.traverse(head) do
    local t = get_type(n)
    if t == LIST or t == LIST_DISC then
      color = traverse(n, color, dry)
    elseif t == LIST_LEADERS then
      local color_after = traverse(n.leader, color, DRY_TRUE)
      if color == color_after then
        traverse(n.leader, color, DRY_FALSE or dry)
      else
        traverse(n.leader, '', DRY_FALSE or dry)
        color = ''
      end
    elseif t == COLOR then
      local v = node.has_attribute(n, attribute)
      if v then
        local newColor = map[v]
        if newColor ~= color then
          color = newColor
          if dry == DRY_FALSE then
            local newNode
            if ifpdf then
              newNode = node.new(WHATSIT, PDFLITERAL)
              newNode.mode = mode
              newNode.data = color
            else
              newNode = node.new(WHATSIT, SPECIAL)
              newNode.data = prefix .. color
            end
            head = node.insert_before(head, n, newNode)
          end
        end
      end
    elseif t == NOCOLOR then
      color = ''
    end
  end
  if get_type(list) == LIST then
    list.head = head
  else
    list.replace = head
  end
  return color
end
function luacolor.process(box)
  local color = ""
  local list = tex.getbox(box)
  traverse(list, color, DRY_FALSE)
end

if luatexbase.callbacktypes.pre_shipout_filter then
  luatexbase.add_to_callback("pre_shipout_filter", function(list)
    traverse(list, "", DRY_FALSE)
    return true
  end, "luacolor.process")
end
if luaotfload.set_colorhandler then
  local set_attribute = node.direct.set_attribute
  luaotfload.set_colorhandler(function(head, n, color)
    set_attribute(n, attribute, luacolor.getvalue(color))
    return head, n
  end)
end
-- 
--  End of File `luacolor.lua'.
