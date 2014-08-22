# Print out all the subtypes of x recursively.
function subtypes_rec(x::DataType; depth=0, seen=Set{DataType}())
    indent = " "^2depth

    if x in seen
        # The type graph is not a tree in Julia, as Any is a subtype of itself,
        # so we have to be careful about cycles.
        print_with_color(:red, "$indent$x [cycle]\n")

        return
    end

    push!(seen, x)
    print_with_color(x.abstract ? :cyan : :bold, "$indent$x\n")

    for s in subtypes(x)
        subtypes_rec(s, depth=depth+1, seen=seen)
    end

    nothing
end
