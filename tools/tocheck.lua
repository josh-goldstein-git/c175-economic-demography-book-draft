-- Highlight claims marked [text]{.tocheck note="..."}.
-- HTML: yellow highlight (styles.css), note shown after the claim.
-- PDF: claim in dark orange text (wraps normally); note as a footnote.
function Span(el)
  if not el.classes:includes("tocheck") then return nil end
  local note = el.attributes["note"]
  if quarto.doc.is_format("html") then
    if note then el.attributes["data-note"] = note end
    return el
  elseif quarto.doc.is_format("latex") then
    local out = { pandoc.RawInline("latex", "\\textcolor[HTML]{B35900}{") }
    for _, v in ipairs(el.content) do table.insert(out, v) end
    table.insert(out, pandoc.RawInline("latex", "}"))
    if note then table.insert(out, pandoc.Note({ pandoc.Plain({ pandoc.Str("Check: " .. note) }) })) end
    return out
  end
  return el
end
