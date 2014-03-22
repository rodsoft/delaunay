-- Delaunay Triangulation
-- Author: Rodolfo Lima
-- Creation:    2014/03/21
-- Last update: 2014/03/21

local M = {}

M.setmetatable = setmetatable
M.type = type
M.pairs = pairs
M.tostring = tostring
M.assert = assert
M.math = math
M.require = require
M.ipairs = ipairs
M.io = io
M.file = file
M.hedge_module = require "hedge.hedge"
M.Mesh = M.hedge_module.Mesh
M.print = print

_ENV = M

-- DEBUG STUFF -----------------------------------------------------{{{
debug_mode = false
output_algo = false
enable_trace = true

function trace(...)
    for _,s in ipairs({...}) do
        if _ > 1 then
            io.stderr:write(" ")
        end
        io.stderr:write(s)
    end
    io.stderr:write("\n")
end

symbolic_supertriangle = false

function ps_header(out, min, max)
    min = min - (max-min)/10
    max = max + (max-min)/10

    out:write("%!PS-Adobe-3.0\n",
             ("%%%%BoundingBox: %f %f %f %f\n")
                :format(min.x,min.y,max.x,max.y),
              "%%EndComments\n",
              "%%BeginSetup\n",
              ("/minx %f def /miny %f def /maxx %f def /maxy %f def\n")
                :format(min.x,min.y,max.x,max.y),
[[
/W 612 def /H 792 def
/w maxx minx sub def /h maxy miny sub def
/radius 1 W div def
/red {1 0 0} def
/green {0 1 0} def
/blue {0 0 1} def
/white {1 1 1} def
/black {0 0 0} def
/p %% p(x,y)
{
    newpath radius 0 360 arc fill
} def
/line %% line(x1,y1,x2,y2)
{
    newpath 4 2 roll moveto lineto stroke
} def

/vtxfont /Times-Roman findfont 20 h mul H div scalefont def
/titlefont /Times-Roman findfont 30 scalefont def

/print
{
    3 1 roll moveto w h div 1 scale show h w div 1 scale
} def

0 H W sub 2 div translate
W dup scale
1 w div 1 h div scale
minx neg miny neg translate
/meshctm matrix currentmatrix def

matrix defaultmatrix setmatrix

%%EndSetup
]],
    "\n")
end

local function output_ps(P, mesh, dag, out, title)
    assert(out ~= nil)
    assert(P ~= nil)

    if title ~= nil then
        out:write(("titlefont setfont (%s) dup stringwidth pop W exch sub 2 div H moveto show\n")
                    :format(title))
    end

    local in_red = false

    out:write([[

0 0 1 setrgbcolor
1 W div setlinewidth
meshctm setmatrix
vtxfont setfont

]])

    math.randomseed(666)

    if dag ~= nil then
        local function printer(node)
            if node.children ~= nil then
                for _,child in pairs(node.children) do
                    printer(child)
                end
            else
                assert(P ~= nil)
                assert(out ~= nil)

                out:write("newpath\n",
                          P[node[1]].x," ",P[node[1]].y," moveto\n",
                          P[node[2]].x," ",P[node[2]].y," lineto\n",
                          P[node[3]].x," ",P[node[3]].y," lineto\n",
                          math.random()," ",math.random()," ",math.random()," sethsbcolor\n",
                          "closepath fill\n")
            end
        end

        printer(dag)
    end

    for _,e in pairs(mesh.edges) do
        if e.vtx.id < 0 or e.next.vtx.id < 0 then
            if not in_red then
                out:write("1 0 0 setrgbcolor\n")
                in_red = true
            end
        elseif in_red then
            out:write("0 0 1 setrgbcolor\n")
            in_red = false
        end

        out:write(P[e.vtx.id].x, " ",P[e.vtx.id].y,
                  " ", P[e.next.vtx.id].x, " ", P[e.next.vtx.id].y, " line\n")
    end

    if #P < 10 then
        out:write("0 0 0 setrgbcolor\n")
        for _,p in pairs(P) do
            out:write(p.x," ",p.y," (",_,") print\n")
        end
    end
end

local mesh_output_stream
local curpage
-- output_mesh(fname or stream, P, mesh, dag, msg)
-- output_mesh(P, mesh, dag, msg)
function output_mesh(out, P, mesh, dag, msg)
    if type(out)~="string" and (type(out)~="userdata" or out.write == nil) then
        assert(msg == nil)
        P, mesh, dag, msg = out, P, mesh, dag

        if type(mesh_output_stream) == "string" then
            out = io.open(mesh_output_stream, "a+") -- append
        else
            out = mesh_output_stream
        end
        curpage = curpage + 1
    else
        mesh_output_stream = out
        if type(out) == 'string' then
            out = io.open(out, "w+") -- create a new file
        else
            out = mesh_output_stream
        end

        ps_header(out, bounds(P))
        curpage = 1
    end

    assert(P ~= nil)
    assert(mesh ~= nil)

    out:write("%%Page ",curpage," ",curpage,"\n")

    out:write("gsave\n")
    output_ps(P,mesh,dag,out,msg)
    out:write("grestore\n")

    out:write("showpage\n")

    if out ~= mesh_output_stream then
        out:close()
    end

    algo_file_created = true
end
--}}}

-- POINT ---------------------------------------------------------------{{{

Point = {}
function Point:new(x,y)
    local o = {x=x, y=y}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Point.__sub(a,b)
    return Vector:new(a.x-b.x, a.y-b.y)
end
function Point.__eq(a,b)
    return a.x==b.y and a.y==b.y
end
-- lexicographical order
function Point.__lt(a,b)
    if a.x==b.x then
        return a.y < b.y
    else
        return a.x < b.x
    end
end
function Point.__le(a,b)
    if a.x==b.x then
        return a.y <= b.y
    else
        return a.x <= b.x
    end
end
function Point:__tostring()
    return "("..self.x..";"..self.y..")"
end

local function are_collinear(a, b, c)
    local tol = math.abs(math.max(a.x,b.x,c.x,a.y,b.y,c.y))*1e-6

    return math.abs(cross(Vector:new(b.x,b.y)-a,Vector:new(c.x,c.y)-a)) < tol
end
--}}}

-- VECTOR ---------------------------------------------------------------{{{

Vector = { __sub = Point.__sub,
           __eq  = Point.__eq,
           __lt  = Point.__lt,
           __le  = Point.__le }

function Vector:new(x,y)
    local o = {x=x, y=y}
    setmetatable(o, self)
    self.__index = self
    return o
end
function Vector.__add(a,b)
    return Vector:new(a.x+b.x, a.y+b.y)
end
function Vector.__unm(a)
    return Vector:new(-a.x, -a.y)
end
function Vector.__mul(a,b)
    if type(a) == 'number' then
        return Vector:new(a*b.x, a*b.y)
    elseif type(b) == 'number' then
        return Vector:new(a.x*b, a.y*b)
    else
        error("Invalid arguments to vector multiplication: "..type(a).."*"..type(b))
    end
end
function Vector.__div(a,b)
    if type(b) == 'number' then
        return Vector:new(a.x/b, a.y/b)
    else
        error("Invalid arguments to vector division")
    end
end
function Vector:__tostring()
    return "("..self.x..","..self.y..")"
end
function dot(v1,v2)
    return v1.x*v2.x + v1.y*v2.y
end
function cross(v1,v2)
    return v1.x*v2.y - v1.y*v2.x
end
function norm2(a)
    return dot(a,a)
end
function norm(a)
    return math.sqrt(norm2(a))
end
function unit(a)
    return a / norm(a)
end
function angle(a,b)
    return math.acos(dot(unit(a), unit(b)))
end

--}}}

------------------------------------------------------------

local function top_point(P)
    local max = Point:new(-1e10,-1e10)
    local id0

    for i=1,#P do
        if P[i].y > max.y or P[i].y == max.y and P[i].x > max.x then
            max = P[i]
            id0 = i
        end
    end

    if debug_mode then
        assert(id0 ~= nil)
    end

    return id0
end

local function ccw(P, a, b, p)
    if symbolic_supertriangle and (a < 0 or b < 0) then
        if a == -1 and b == -2 then
            return 1
        elseif a == -2 and b == -1 then
            return -1
        elseif a == -1 then
            if not Point.__le(P[p],P[b]) then
                return 1
            else
                return -1
            end
        elseif b == -2 then
            if not Point.__le(P[p],P[a]) then
                return 1
            else
                return -1
            end
        elseif a == -2 then
            if not Point.__le(P[p],P[b]) then
                return -1
            else
                return 1
            end
        elseif b == -1 then
            if not Point.__le(P[p],P[a]) then
                return -1
            else
                return 1
            end
        end
    end

    a = P[a]
    b = P[b]
    p = P[p]

    local d00, d01 = p.x-a.x, p.y-a.y
    local d10, d11 = b.x-a.x, b.y-a.y

    return d00*d11 - d01*d10
end

local function inside_triangle(P, tri, p)
    local a,b,c = tri[1], tri[2], tri[3]
    return ccw(P,a,b,p) <= 0 and ccw(P,b,c,p) <= 0 and ccw(P,c,a,p) <= 0
end

local function strictly_inside_triangle(P, tri, p)
    local a,b,c = tri[1], tri[2], tri[3]
    return ccw(P,a,b,p) < 0 and ccw(P,b,c,p) < 0 and ccw(P,c,a,p) < 0
end

local function find_triangle(P, node, p)
    if not inside_triangle(P, node, p) then
        return nil
    elseif node.children == nil then -- leaf node?
        return node
    end

    for i,child in pairs(node.children) do
        local tri = find_triangle(P, child, p)
        if tri ~= nil then
            return tri
        end
    end

    if debug_mode then
        assert(false, "must have found a triangle containing "..p.." ("..node[1].." "..node[2].." "..node[3]..")")
    end
end

local function circumcircle(a,b,c)
    local D = (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y)) * 2

    local x = norm2(a) * (b.y - c.y) +
              norm2(b) * (c.y - a.y) +
              norm2(c) * (a.y - b.y)

    local y = norm2(a) * (c.x - b.x) +
              norm2(b) * (a.x - c.x) +
              norm2(c) * (b.x - a.x)

    local center = Point:new((x / D), (y / D))


    local la,lb,lc = norm(Vector:new(a.x,a.y)-b),
                     norm(Vector:new(b.x,b.y)-c),
                     norm(Vector:new(c.x,c.y)-a)

    -- area*4 of ijk triangle (use Heron's formula)
    local area4_2 = (la+lb+lc)*(la+lb-lc)*(la-lb+lc)*(-la+lb+lc)
    local radius2 = (la*lb*lc)^2/area4_2

    return center,radius2
end

local function is_illegal_edge(P, edge, p)

    local i,j,k = edge.vtx.id,
                  edge.next.vtx.id,
                  edge.opp.prev.vtx.id

    --print("is illegal ",edge,i,j,k,p)
    if debug_mode then
        assert(p ~= edge.vtx.id and p ~= edge.next.vtx.id)
    end

    -- border edge is always legal
    if edge.face == nil or edge.opp.face == nil then
        return false
    end

    local i,j,k = edge.vtx.id,
                  edge.next.vtx.id,
                  edge.opp.prev.vtx.id

    if debug_mode then
        assert(i ~= j)
        assert(j ~= k)
        assert(k ~= p)
        assert(p ~= i)
    end

    if symbolic_supertriangle and (i<0 or j<0 or k<0 or p<0) then
        -- at most one of i,j is < 0
        assert(i>=0 and j>=0 or (i<0) ~= (j<0)) -- xor
        -- at most one of k,p is < 0
        assert(k>=0 and p>=0 or (k<0) ~= (p<0)) -- xor
        return math.min(k,p) >= math.min(i,j)
    end

    local center,radius2 = circumcircle(P[i], P[j], P[k])

    -- if p is in the interior of circumcircle, edge is illegal
    trace(norm2(center-P[p]), radius2)
    return norm2(center-P[p]) < radius2
end


local function legalize_edge(mesh, P, dag, edge, p, trinode)
    --print("legalize ",edge,p)
    if debug_mode then
        assert(edge.vtx.id ~= p)
        assert(edge.next.vtx.id ~= p)

        assert(edge.prev.vtx.id == p)
        assert(edge.next.next.vtx.id == p)
    end

    if is_illegal_edge(P, edge, p) then
        local msg
        if debug_mode then
            msg = "flip "..tostring(edge).." p="..p
        end
        local did = mesh:flip_edge(edge)
        if debug_mode then
            assert(did, "should have flipped "..tostring(edge))
        end

        local node, node_opp = trinode[edge.face.id], trinode[edge.opp.face.id]

        local child = trinode:create(node,edge)
        local child_opp = trinode:create(node,edge.opp)

        if node_opp.children == nil then
            node_opp.children = {}
        end
        node_opp.children[#node_opp.children+1] = child
        node_opp.children[#node_opp.children+1] = child_opp

        if output_algo then
            output_mesh(P,mesh,dag, msg)
        end
        if debug_mode then
            assert(not is_illegal_edge(P, edge, edge.prev.vtx.id), "should be a legal edge by now")
        end

        local e1 = edge.next
              e2 = edge.opp.prev
        local a = e2.vtx.id
        local b = e2.next.vtx.id
        legalize_edge(mesh, P, dag, e1, p, trinode)

        if e2.vtx.id == a and e2.next.vtx.id == b then
            legalize_edge(mesh, P, dag, e2, p, trinode)
        end
    end
end

local function add_vertex_interior(mesh, face, p)
    return mesh:split_face(face, p)
end

local function add_vertex_edge(mesh, edge, p)
    local vtx = mesh:split_edge(edge, p)

    -- then we create triangles on edge's face, linking the added vertex to
    -- each vertex (minus 2)

    if edge.face ~= nil then
        mesh:triangulate(edge)
    end

    -- same thing on edge.opp.face

    if edge.opp.face ~= nil then
        mesh:triangulate(edge.opp.next)
    end

    return vtx
end

function bounds(P)
    local max,min = Point:new(-1e10,-1e10), Point:new(1e10,1e10)

    for _,p in pairs(P) do
        min.x = math.min(min.x, p.x)
        min.y = math.min(min.y, p.y)
        max.x = math.max(max.x, p.x)
        max.y = math.max(max.y, p.y)
    end

    return min,max
end

local function mega_triangle(P)
    local min,max = bounds(P)

    local size = max-min
    min = min - size/10
    max = max + size/10

    local w,h = max.x-min.x, max.y-min.y
    local d = w/2
    local c = w*h/(2*d)

    return Point:new(min.x-d,min.y),
           Point:new(max.x+d,min.y),
           Point:new((min.x+max.x)/2, max.y+c)
end

function randomize(P)
    --  http://en.wikipedia.org/wiki/Fisher-Yates_shuffle
    for i=#P,2,-1 do
        local j = math.random(i)
        P[i],P[j] = P[j],P[i]
    end
    return P
end

function triangulate(P)
    local trinode = {}
    function trinode:create(parent,e)
        local node = {tri = e.face}
        node[1] = e.vtx.id
        node[2] = e.next.vtx.id
        node[3] = e.prev.vtx.id
        if parent.children == nil then
            parent.children = {}
        end
        parent.children[#parent.children + 1] = node

        trinode[e.face.id] = node
        return node
    end

    local mesh = Mesh:new()

    local f0, id0

    if symbolic_supertriangle then
        id0 = top_point(P)
        f0 =  mesh:add_face(id0,-2,-1)
    else
        P[-1], P[-2], P[-3] = mega_triangle(P)
        f0 =  mesh:add_face(-1,-2,-3)
    end

    local root = {tri = f0}
    root[1] = f0[1].vtx.id
    root[2] = f0[2].vtx.id
    root[3] = f0[3].vtx.id
    trinode[f0.id] = root

    if output_algo then
        output_mesh("algo.ps", P,mesh,root,"super triangle")
    end

    for p,pr in pairs(P) do
        if p <= 0 or p == id0 then
            goto continue
        end

        local node = find_triangle(P, root, p)
        if debug_mode then
            assert(node ~= nil, "must have found a triangle")
        end

        if strictly_inside_triangle(P, node, p) then
            local msg
            if debug_mode then
                msg = "split "..tostring(node.tri).." with "..p
            end
            local vtx = add_vertex_interior(mesh, node.tri, p)

            local edges = {}
            for e in vtx:out_edges() do
                edges[#edges+1] = e
                trinode:create(node,e)
            end

            if output_algo then
                output_mesh(P,mesh,root,msg)
            end

            if debug_mode then
                assert(#node.children == 3, "wrong number of children: "..#node.children)
            end


            for i=1,#edges do
                legalize_edge(mesh, P, root, edges[i].next, p, trinode)
            end
        else
            local a,b,c = P[node[1]], P[node[2]], P[node[3]]
            local edge
            if are_collinear(a,b,pr) then
                edge = node.tri[1]
            elseif are_collinear(b,c,pr) then
                edge = node.tri[2]
            else
                if debug_mode then
                    assert(are_collinear(c,a,pr))
                end
                edge = node.tri[3]
            end

            local opp_tri = edge.opp.face
            local opp_vtx, opp_node

            if opp_tri ~= nil then
                opp_vtx = edge.opp.prev.vtx
                opp_node = trinode[opp_tri.id]
                if debug_mode then
                    assert(opp_node ~= nil)
                end
            end

            local msg
            if debug_mode then
                msg = "split "..tostring(edge)
            end
            local vtx = add_vertex_edge(mesh, edge, p)

            local edges = {}
            for e in vtx:out_edges() do
                if e.face ~= nil then -- no need to process border edges
                    edges[#edges+1] = e

                    if e.prev.vtx == opp_vtx then
                        if opp_node ~= nil then
                            trinode:create(opp_node,e)
                        end
                    else
                        trinode:create(node,e)
                    end
                end
            end

            if output_algo then
                output_mesh(P,mesh,root,msg)
            end

            for i=1,#edges do
                legalize_edge(mesh, P, root, edges[i].next, p, trinode)
            end
        end

        ::continue::
    end

    for i=-3,-1 do
        mesh:remove_vertex(i)
        P[i] = nil
        if output_algo then
            output_mesh(P,mesh,nil,"Remove vertex "..i)
        end
    end

    return mesh, P
end

return M
