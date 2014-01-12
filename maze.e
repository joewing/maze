indexing
    description: "Maze generator in Eiffel"
    author: "Joe Wingbermuehle"
    date: "2014-01-12"

class
    MAZE

create {ANY}
    make

feature {ANY}

    -- Display the maze.
    show is
        local
            x, y: INTEGER
        do
            from y := 0 until y = height loop
                from x := 0 until x = width loop
                    if data.item(y * width + x) then
                        io.put_string("[]")
                    else
                        io.put_string("  ")
                    end
                    x := x + 1
                end
                io.put_new_line
                y := y + 1
            end
        end

feature {}

    width:  INTEGER is 39   -- Width; must be odd.
    height: INTEGER is 23   -- Height; must be odd.
    data:   ARRAY[BOOLEAN]
    rng:    MINIMAL_RANDOM_NUMBER_GENERATOR is
        do
            !!Result.make
        end

    -- Initialize the maze array.
    init is
        local
            x, y: INTEGER
        do
            !!data.make(0, width * height - 1)
            from y := 0 until y = height loop
                from x := 0 until x = width loop
                    if y = 0 or y = height - 1 then
                        data.put(False, y * width + x)
                    elseif x = 0 or x = width - 1 then
                        data.put(False, y * width + x)
                    else
                        data.put(True, y * width + x)
                    end
                    x := x + 1
                end
                y := y + 1
            end
        end

    -- Get a random direction between 0 and 3 inclusive.
    random_direction: INTEGER is
        do
            Result := rng.last_integer(4) - 1
            rng.next
        ensure
            Result >= 0 and Result < 4
        end

    -- Carve starting at x, y.
    carve(x, y: INTEGER) is
        local
            d, i:   INTEGER
            dx, dy: INTEGER
            tx, ty: INTEGER
            nx, ny: INTEGER
        do
            data.put(False, y * width + x)
            d := random_direction
            from i := 0 until i = 4 loop
                dx := 0; dy := 0
                inspect (d + i) & 3
                    when 0 then dx := 1
                    when 1 then dx := -1
                    when 2 then dy := 1
                    when 3 then dy := -1
                end
                tx := x + dx; ty := y + dy
                if data.item(ty * width + tx) then
                    nx := tx + dx; ny := ty + dy
                    if data.item(ny * width + nx) then
                        data.put(False, ty * width + tx)
                        carve(nx, ny)
                    end
                end
                i := i + 1
            end
        end

    -- Generate a random maze.
    generate is
        do
            init
            carve(2, 2)
            data.put(False, 1 * width + 2)
            data.put(False, (height - 2) * width + (width - 3))
        end

    -- Generate and display a random maze.
    make is
        do
            generate
            show
        end

end
