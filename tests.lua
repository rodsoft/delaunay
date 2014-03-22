local d = require "delaunay"
local gold = require "gold"

d.debug_mode = true
d.hedge_module.debug_mode = true
d.hedge_module.enable_trace = true
d.enable_trace = true
d.symbolic_supertriangle = false

function gen_points(N)
    local P = {}
    for i=1,N do
        P[#P+1] = d.Point:new(math.random(), math.random())
    end

    return P
end

local function pteq(a,b)
    return a.x == b.x and a.y == b.y
end

local function ptlt(a,b)
    if a.x == b.x then 
        return a.y < b.y 
    else 
        return a.x < b.x
    end 
end

local function ptstring(a)
    return tostring(a.x)..','..tostring(a.y)
end

local function remove_duplicated_edges(edges)
    local unique_edges = {}
    for _,e in pairs(edges) do
        local a, b
        if ptlt(e[1],e[2]) then
            a, b = e[1], e[2]
        else
            a, b = e[2], e[1]
        end

        unique_edges[ptstring(a)..","..ptstring(b)] = {a, b}
    end

    local sorted_edges = {}
    for _,e in pairs(unique_edges) do
        sorted_edges[#sorted_edges+1] = e
    end

    table.sort(sorted_edges, function(a,b) 
        if pteq(a[1],b[1]) then
            return ptlt(a[2],b[2])
        else
            return ptlt(a[1],b[1])
        end
    end)

    return sorted_edges
end


function output_my(P, out)
    out = out or io.stdout

    local mesh = d.triangulate(P)

    local edges = {}
    for _,e in pairs(mesh.edges) do
        edges[#edges+1] = {P[e.vtx.id],P[e.next.vtx.id]}
    end

    edges = remove_duplicated_edges(edges)

    d.ps_header(out, d.bounds(P))
    out:write("meshctm setmatrix 1 W div setlinewidth\n")

    for _,e in ipairs(edges) do
        out:write(e[1].x, " ",e[1].y, " ", e[2].x, " ", e[2].y, " line\n")
    end

    out:write("showpage\n")
end

function output_gold(iP, out)
    out = out or io.stdout

    local P = {}
    for i=1,#iP do
        P[#P+1] = gold.Point(iP[i].x, iP[i].y)
    end

    local mesh = gold.triangulate(P)

    local edges = {}
    for _,t in pairs(mesh) do
        edges[#edges+1] = {t.p1,t.p2}
        edges[#edges+1] = {t.p2,t.p3}
        edges[#edges+1] = {t.p3,t.p1}
    end

    edges = remove_duplicated_edges(edges)

    d.ps_header(out, d.bounds(P))
    out:write("meshctm setmatrix 1 W div setlinewidth\n")

    for _,e in ipairs(edges) do
        out:write(e[1].x," ",e[1].y," ", e[2].x, " ", e[2].y, " line\n")
    end

    out:write("showpage\n")
end

function benchmark(N)
    d.debug_mode = false
    d.hedge_module.debug_mode = false
    d.hedge_module.enable_trace = false
    d.enable_trace = false

    local P = gen_points(N)

    local clk = os.clock()
    local mesh
    mesh,P = d.triangulate(P)
    print("Elapsed: ",os.clock()-clk)
end

function get_points(val)
    d.output_algo = true
    if val == "square" then
        return {{x=0,y=0},{x=1,y=1e-5},{x=0,y=1},{x=1,y=1}}
    elseif val == "on_edge" then
        return {{x=0,y=0},{x=1,y=0},{x=1,y=1},{x=0.5,y=0.5}}
    else
        d.output_algo = false
        return gen_points(tonumber(val))
    end
end

math.randomseed(666)

if arg[1] == "bench" then
    print("Benchmark N="..arg[2])
    benchmark(tonumber(arg[2]))
elseif arg[1] == "gold" then
    output_gold(get_points(arg[2]))
elseif arg[1] == "my" then
    output_my(get_points(arg[2]))
end

