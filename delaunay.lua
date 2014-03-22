local hedge = require "hedge.hedge"
local gold = require "gold"

local Mesh = hedge.Mesh
local trace = hedge.trace

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

%%EndSetup
]],
    "\n")
end

function output_ps(P, mesh, dag, out, title)
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

    if dag ~= nil then
        local function printer(node)
            if node.children ~= nil then
                for _,child in pairs(node.children) do
                    printer(child)
                end
            else
                out:write("newpath\n",
                          P[node[1]].x," ",P[node[1]].y," moveto\n",
                          P[node[2]].x," ",P[node[2]].y," lineto\n",
                          P[node[3]].x," ",P[node[3]].y," lineto\n",
                          math.random()," 1 1 sethsbcolor\n",
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

local idx = 1
function printmesh(P, mesh, dag, msg)
    do return end

    local out
    if idx == 1 then
        out = io.open("algo.ps","w+")

        ps_header(out, bounds(P))
    else
        out = io.open("algo.ps","a+")
    end

    out:write("%%Page ",idx," ",idx,"\n")

    out:write("gsave\n")

    output_ps(P,mesh,dag,out,msg)

    out:write("grestore\n","showpage\n")

    out:close()

    idx = idx+1
end


-- POINT ---------------------------------------------------------------

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

function are_collinear(a, b, c)
    local tol = math.abs(math.max(a.x,b.x,c.x,a.y,b.y,c.y))*1e-6

    return math.abs(cross(b-a,c-a)) < tol
end
                

-- VECTOR ---------------------------------------------------------------

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

------------------------------------------------------------

function top_point(P)
    local max = Point:new(-1e10,-1e10)
    local id0

    for i=1,#P do
        if P[i].y > max.y or P[i].y == max.y and P[i].x > max.x then
            max = P[i]
            id0 = i
        end
    end

    assert(id0 ~= nil)

    return id0
end

function ccw(P, a, b, p)
    --[[
    if a < 0 or b < 0 then
        if a == -1 and b == -2 then
            return 1
        elseif a == -2 and b == -1 then
            return -1
        elseif a == -1 then
            if P[p] > P[b] then
                return 1
            else
                return -1
            end
        elseif b == -2 then
            if P[p] > P[a] then
                return 1
            else
                return -1
            end
        elseif a == -2 then
            if P[p] > P[b] then
                return -1
            else
                return 1
            end
        elseif b == -1 then
            if P[p] > P[a] then
                return -1
            else
                return 1
            end
        end
    end
    --]]

    a = P[a]
    b = P[b]
    p = P[p]

    local d00, d01 = p.x-a.x, p.y-a.y
    local d10, d11 = b.x-a.x, b.y-a.y

    return d00*d11 - d01*d10
end

function inside_triangle(P, tri, p)
    local a,b,c = tri[1], tri[2], tri[3]
    return ccw(P,a,b,p) <= 0 and ccw(P,b,c,p) <= 0 and ccw(P,c,a,p) <= 0
end

function strictly_inside_triangle(P, tri, p)
    local a,b,c = tri[1], tri[2], tri[3]
    return ccw(P,a,b,p) < 0 and ccw(P,b,c,p) < 0 and ccw(P,c,a,p) < 0
end

function find_triangle(P, node, p)
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

    assert(false, "must have found a triangle containing "..p.." ("..node[1].." "..node[2].." "..node[3]..")")
end

function circumcircle(a,b,c)
    local D = (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y)) * 2

    local x = norm2(a) * (b.y - c.y) +
              norm2(b) * (c.y - a.y) +
              norm2(c) * (a.y - b.y)

    local y = norm2(a) * (c.x - b.x) +
              norm2(b) * (a.x - c.x) +
              norm2(c) * (b.x - a.x)

    local center = Point:new((x / D), (y / D))


    local la,lb,lc = norm(a-b), norm(b-c), norm(c-a)

    -- area*4 of ijk triangle (use Heron's formula)
    local area4 = math.sqrt((la+lb+lc)*(la+lb-lc)*(la-lb+lc)*(-la+lb+lc))
    local radius = (la*lb*lc)/area4

    return center,radius
end

function is_illegal_edge(P, edge, p)

    local i,j,k = edge.vtx.id,
                  edge.next.vtx.id,
                  edge.opp.prev.vtx.id

    --print("is illegal ",edge,i,j,k,p)
    assert(p ~= edge.vtx.id and p ~= edge.next.vtx.id)

    -- border edge is always legal
    if edge.face == nil or edge.opp.face == nil then
        return false
    end

    local i,j,k = edge.vtx.id,
                  edge.next.vtx.id,
                  edge.opp.prev.vtx.id

    assert(i ~= j)
    assert(j ~= k)
    assert(k ~= p)
    assert(p ~= i)

    --[[
    if i<0 or j<0 or k<0 or p<0 then
        -- at most one of i,j is < 0
        assert(i>=0 and j>=0 or (i<0) ~= (j<0)) -- xor
        -- at most one of k,p is < 0
        assert(k>=0 and p>=0 or (k<0) ~= (p<0)) -- xor
        return math.min(k,p) >= math.min(i,j)
    end
    --]]

    local center,radius = circumcircle(P[i], P[j], P[k])
          
    -- if p is in the interior of circumcircle, edge is illegal
    return norm2(center-P[p]) <= radius*radius
end


function legalize_edge(mesh, P, dag, edge, p, trinode)
    --print("legalize ",edge,p)
    assert(edge.vtx.id ~= p)
    assert(edge.next.vtx.id ~= p)

    assert(edge.prev.vtx.id == p)
    assert(edge.next.next.vtx.id == p)

    if is_illegal_edge(P, edge, p) then
        local msg = "flip "..tostring(edge).." p="..p
        local did = mesh:flip_edge(edge)
        assert(did)

        local node, node_opp = trinode[edge.face.id], trinode[edge.opp.face.id]

        local child = trinode:create(node,edge)
        local child_opp = trinode:create(node,edge.opp)

        if node_opp.children == nil then
            node_opp.children = {}
        end
        node_opp.children[#node_opp.children+1] = child
        node_opp.children[#node_opp.children+1] = child_opp

        printmesh(P,mesh,dag, msg)

        assert(not is_illegal_edge(P, edge, edge.prev.vtx.id), "should be a legal edge by now")

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

function add_vertex_interior(mesh, face, p)
    return mesh:split_face(face, p)
end

function add_vertex_edge(mesh, edge, p)
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

function mega_triangle(P)
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

function triangulate(P)
    P = randomize(P)

    --[[
    local id0 = top_point(P)
    P[0],P[id0] = P[id0],P[0]

    local mesh = Mesh:new()

    local root = {}
    root[1] = 0
    root[2] = -2
    root[3] = -1

    local f0  = mesh:add_face(0,-2,-1)
    --]]

    local trinode = {}
    function trinode:create(parent,e)
        local node = {tri = e.face}
        node[1] = e.vtx.id
        node[2] = e.next.vtx.id
        node[3] = e.prev.vtx.id
        if parent ~= nil then
            node.level = parent.level+1
            if parent.children == nil then
                parent.children = {}
            end
            parent.children[#parent.children + 1] = node
        else
            node.level = 0
        end

        trinode[e.face.id] = node
        return node
    end

    P[-1], P[-2], P[-3] = mega_triangle(P)

    local mesh = Mesh:new()
    local f0 =  mesh:add_face(-1,-2,-3)

    local root = trinode:create(nil,f0[1])
    
    printmesh(P,mesh,root,"super triangle")

    for p,pr in pairs(P) do
        if p <= 0 then
            goto continue
        end

        local node = find_triangle(P, root, p)
        assert(node ~= nil, "must have found a triangle")

        if strictly_inside_triangle(P, node, p) then
            local msg = "split "..tostring(node.tri).." with "..p
            local vtx = add_vertex_interior(mesh, node.tri, p)

            local edges = {}
            for e in vtx:out_edges() do
                edges[#edges+1] = e
                trinode:create(node,e)
            end

            assert(#node.children == 3, "wrong number of children: "..#node.children)

            printmesh(P,mesh,root,msg)

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
                assert(are_collinear(c,a,pr))
                edge = node.tri[3]
            end

            local opp_tri = edge.opp.face
            local opp_vtx, opp_node

            if opp_tri ~= nil then
                opp_vtx = edge.opp.prev.vtx
                opp_node = trinode[opp_tri.id]
                assert(opp_node ~= nil)
            end

            local msg = "split "..tostring(edge)
            local vtx = add_vertex_edge(mesh, edge, p)

            for e in vtx:out_edges() do
                edges[#edges+1] = e

                if e.prev.vtx == opp_vtx then
                    if opp_node ~= nil then
                        trinode:create(opp_node,e)
                    end
                else
                    trinode:create(node,e)
                end
            end

            printmesh(P,mesh,root,msg)

            for i=1,#edges do
                legalize_edge(mesh, P, root, edges[i].next, p, trinode)
            end
        end

        ::continue::
    end

    for i=-3,-1 do
        mesh:remove_vertex(i)
        P[i] = nil
        printmesh(P,mesh,nil,"Remove vertex "..i)
    end

    return mesh, P
end

function randomize(P)
    for i=1,#P do
        local j = i+math.floor(math.random()*(#P-i))
        P[i],P[j] = P[j],P[i]
    end
    return P
end

function output_gold(P, out)
    local gP = {}
    for _,p in pairs(P) do
        gP[#gP+1] = gold.Point(p.x,p.y)
    end

    local mesh = gold.triangulate(gP)

    ps_header(out, bounds(P))
    out:write("meshctm setmatrix 1 W div setlinewidth\n")

    for _,t in pairs(mesh2) do
        out:write(t.p1.x," ",t.p1.y," ", t.p2.x, " ", t.p2.y, " line\n")
        out:write(t.p2.x," ",t.p2.y," ", t.p3.x, " ", t.p3.y, " line\n")
        out:write(t.p3.x," ",t.p3.y," ", t.p1.x, " ", t.p1.y, " line\n")
    end

    out:write("showpage\n")
end

function benchmark(N)
    P = {}
    for i=1,N do
        P[#P+1] = Point:new(math.random(),math.random())
    end

    local clk = os.clock()
    mesh,P = triangulate(P)
    print("Elapsed: ",os.clock()-clk)
end

benchmark(10000)

