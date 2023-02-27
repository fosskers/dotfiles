require("fennel").install().dofile("/home/colin/.config/luakit/config.fnl")

local select = require("select")

select.label_maker = function()
  return interleave("arst", "neio")
end
