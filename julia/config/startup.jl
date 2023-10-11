# Print out all the subtypes of x recursively.
function subtypes_rec(x::DataType; depth=0, seen=Base.IdSet{DataType}())
    indent = " "^2depth

    if x in seen
        # The type graph is not a tree in Julia, so we have to be careful about cycles.
        printstyled("$indent$x [cycle]\n"; color=:red)

        return
    end

    seen = copy(seen)
    push!(seen, x)

    color = isabstracttype(x) ? :cyan : :bold
    printstyled("$indent$x\n"; color)

    for s in subtypes(x)
        s isa Type || continue
        s == Function && continue
        subtypes_rec(s; depth=depth+1, seen)
    end

    nothing
end

subtypes_rec(x::UnionAll; kwargs...) = subtypes_rec(x.body; kwargs...)
