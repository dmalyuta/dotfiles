using OhMyREPL
using BenchmarkTools
using Pkg

enable_autocomplete_brackets(false)

"""
Display the entire type hierarchy starting from the specified `roottype`.
"""
function subtypetree(roottype, level=1, indent=4)
    level==1 && println(roottype)
    for s in subtypes(roottype)
        println(join(fill(" ", level*indent))*string(s))
        subtypetree(s, level+1, indent)
    end
end # function

"""
Get the subtypes of a Union.
"""
Base.collect(t::Union{Type, DataType, Union{}}) = __startup__collect(t, [])
__startup__collect(t::Type, list) = t<:Union{} ? push!(list, t) :
    __startup__collect(t.b, push!(list, t.a))
__startup__collect(t::Union{DataType,Core.TypeofBottom}, list) = push!(list, t)

# Activate package if REPL started in a package folder.
if isfile("Project.toml") && isfile("Manifest.toml")
    Pkg.activate(".")
elseif isfile("../Project.toml") && isfile("../Manifest.toml")
    Pkg.activate("..")
end
