# Print out all the subtypes of x recursively.
function subtypes_rec(x::DataType; depth=0, seen=Set{DataType}())
    indent = " "^2depth

    if x in seen
        # The type graph is not a tree in Julia, as Any is a subtype of itself,
        # so we have to be careful about cycles.
        printstyled("$indent$x [cycle]\n"; color=:red)

        return
    end

    push!(seen, x)
    printstyled("$indent$x\n"; color=(x.abstract ? :cyan : :bold))

    for s in subtypes(x)
        s isa DataType || continue
        s == Function && continue
        subtypes_rec(s, depth=depth+1, seen=seen)
    end

    nothing
end
