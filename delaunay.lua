local mesh = require "mesh.mesh"

local Mesh = mesh.Mesh

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
    if a.y==b.y then 
        return a.x < b.x
    else
        return a.y < b.y
    end
end
function Point.__le(a,b) 
    if a.y==b.y then 
        return a.x <= b.x
    else
        return a.y <= b.y
    end
end
function Point:__tostring()
    return "("..self.x..";"..self.y..")"
end

function is_collinear(a, b, c)
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
    return Vector:new(-b.x, -b.y)
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

local idx = 1
function printmesh(mesh, name)
    mesh:output_dot(io.open(idx.."_"..name..".dot","w"))
    idx = idx+1
end

function hull_triangle(P)
    local min, max = Point:new(1e10,1e10), Point:new(-1e10,-1e10)

    for i=1,#P do
        min.x = math.min(min.x, P[i].x)
        min.y = math.min(min.y, P[i].y)
        max.x = math.max(max.x, P[i].x)
        max.y = math.max(max.y, P[i].y)
    end

    local d = (max-min)*10

    local stv0 = Point:new(min.x - d.x, min.y - d.y*3)
    local stv1 = Point:new(min.x - d.x, max.y + d.y)
    local stv2 = Point:new(max.x + d.x*3, max.y + d.y)

    return {stv0, stv2, stv1}
end

function ccw(a, b, p)
    local d00, d01 = p.x-a.x, p.y-a.y
    local d10, d11 = b.x-a.x, b.y-a.y

    return d00*d11 - d01*d10
end

function inside_triangle(P, tri, p)
    local a,b,c,p = P[tri[1]], P[tri[2]], P[tri[3]], P[p]

    return ccw(a,b,p) <= 0 and ccw(b,c,p) <= 0 and ccw(c,a,p) <= 0
end

function strictly_inside_triangle(P, tri, p)
    local a,b,c,p = P[tri[1]], P[tri[2]], P[tri[3]], P[p]
    return ccw(a,b,p) < 0 and ccw(b,c,p) < 0 and ccw(c,a,p) < 0
end

function find_triangle(P, node, p)
    if not inside_triangle(P, node, p) then
        return nil
    elseif node.children == nil then
        return node
    end

    for i,child in pairs(node.children) do
        local tri = find_triangle(P, child, p)
        if tri ~= nil then
            return tri
        end
    end

    assert(false, "must have found a triangle")
end


function is_illegal_edge(P, edge)
    -- border edge is always legal
    if edge.face == nil or edge.opp.face == nil then
        return false
    end

    local pi,pj,pl,pk = 
        P[edge.vtx.id],
        P[edge.next.vtx.id],
        P[edge.next.next.vtx.id],
        P[edge.opp.prev.vtx.id]

    local a1 = angle(pj-pi,pl-pi)
    local a2 = angle(pi-pl,pj-pl)
    local a3 = angle(pl-pj,pi-pj)
    local a4 = angle(pk-pi,pj-pi)
    local a5 = angle(pi-pj,pk-pj)
    local a6 = angle(pj-pk,pi-pk)

    local a1p = angle(pk-pi,pl-pi)
    local a2p = angle(pi-pl,pk-pl)
    local a3p = angle(pl-pk,pi-pk)
    local a4p = angle(pk-pl,pj-pl)
    local a5p = angle(pj-pk,pl-pk)
    local a6p = angle(pl-pj,pk-pj)

    local A,B = math.min(a1,a2,a3,a4,a5,a6),
                math.min(a1p,a2p,a3p,a4p,a5p,a6p)

--    print(pi,pk,pj,pl," -> ",pi,pj,": ",A,B,A < B and "NOT" or "ok")

    return A < B, A, B
end


function legalize_edge(mesh, P, edge, pr)
    if is_illegal_edge(P, edge) then
        mesh:flip_edge(edge)
        printmesh(mesh,"flip_edge")

        assert(not is_illegal_edge(P, edge), "should be a legal edge by now")

        if edge.prev.vtx.id == pr then
            edge = edge.opp
        end
        legalize_edge(mesh, P, edge.next, pr)
        legalize_edge(mesh, P, edge.prev, pr)
    end
end

function triangulate(P)
    local hull = hull_triangle(P)
    P[-1] = hull[1]
    P[-2] = hull[2]
    P[-3] = hull[3]

    local dag = {}
    dag[1] = -1
    dag[2] = -2
    dag[3] = -3

    local mesh = Mesh:new()
    dag.face = mesh:add_face(-1, -2, -3)
    
    local trinodes = {}
    trinodes[dag.face.id] = dag

    printmesh(mesh,"ini")

    for p,pr in pairs(P) do
        if p <= 0 then
            goto continue
        end

        local tri = find_triangle(P, dag, p)
        assert(tri ~= nil, "must have found a triangle")

        if strictly_inside_triangle(P, tri, p) then
            local vtx = mesh:split_face(tri.face, p)
            printmesh(mesh,"split_face")

            tri.children = {}
            for e in vtx:out_edges() do
                legalize_edge(mesh, P, e.next, pr)

                local child = {face = e.face}
                child[1] = e.vtx.id
                child[2] = e.next.vtx.id
                child[3] = e.next.next.vtx.id
                
                tri.children[#tri.children+1] = child

                trinodes[child.face.id] = child
            end
        else
            local face = tri.face
            local a,b,c = P[tri[1]], P[tri[2]], P[tri[3]]
            local edge
            if are_collinear(a,b,pr) then
                edge = face[1]
            elseif are_collinear(b,c,pr) then
                edge = face[2]
            else 
                assert(are_collinear(c,d,pr))
                edge = face[3]
            end

            local opp_vtx = edge.prev.vtx

            local opp_tri = trinodes[edge.opp.face.id]
            assert(opp_tri ~= nil)

            local vtx = mesh:split_edge(edge)
            printmesh(mesh,"split_edge")

            tri.children = {}
            for e in vtx:out_edges() do
                legalize_edge(mesh, P, e.next, pr)

                local child = {face = e.face}
                child[1] = e.vtx.id
                child[2] = e.next.vtx.id
                child[3] = e.next.next.vtx.id
                
                if e.prev.vtx == opp_vtx then
                    tri.children[#tri.children+1] = child
                    trinodes[edge.face.id] = child
                else
                    opp_tri.children[#opp_tri.children+1] = child
                    trinodes[edge.opp.face.id] = child
                end
            end
        end

        ::continue::
    end

    for i=-3,-1 do
        mesh:remove_vertex(i)
        printmesh(mesh,"remvtx_"..i)
        P[i] = nil
    end

    return mesh
end

P = {Point:new(0,0), Point:new(1,.9), Point:new(0,1), Point:new(1,1)}
--P = {Point:new(-10,11), Point:new(1,0), Point:new(31,11), Point:new(1,1)}

--[[
mesh = Mesh:new()
mesh:add_face(1,3,4)
local f = mesh:add_face(3,1,2)
--mesh:flip_edge(f.edge)

illegal, A, B = is_illegal_edge(P, f.edge)
print(A,B)
if illegal then
    print "ILLEGAL"
else
    print "legal"
end

print("FLIP")
mesh:flip_edge(f.edge)

illegal, A, B = is_illegal_edge(P, f.edge)
print(A,B)
if illegal then
    print "ILLEGAL"
else
    print "legal"
end

do return end
--]]

mesh = triangulate(P)

print([[
%!PS-Adobe-3.0
%%BoundingBox: -1 -1 2 2
%%BeginSetup
/W 612 def /H 792 def
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

20 20 translate
W -40 add dup scale
1 W div setlinewidth

%%EndSetup
]])

--print("1 0 0 setrbgcolor")

for _,f in pairs(mesh.faces) do
    for i=1,3 do
        print(P[f[i].vtx.id].x, P[f[i].vtx.id].y, P[f[i%3+1].vtx.id].x, P[f[i%3+1].vtx.id].y, "line")
    end
end

--print("0 0 0 setrbgcolor")
for _,p in pairs(P) do
    print(p.x,p.y,"p")
end

print("showpage")
