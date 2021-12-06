drop function calc_fish(int, int, int[][]);
create or replace function calc_fish (n int, day int, mem int[][])
returns int
language plpgsql
as $$
begin
    -- DOESN'T WORK BECAUSE OF PASS BY VALUE
    raise notice 'cache miss %', array(select x from unnest(mem) as x where x != 0);

    if mem[n][day] != 0 then
        return mem[n][day];
    end if;
    if n >= day then
        return 1;
    end if;

    mem[n][day] := calc_fish(8, day - n - 1, mem) + calc_fish(7, day - n, mem);
    return mem[n][day];
end $$;

do $$
declare
    input int[] = string_to_array('3,4,3,1,2', ',')::int[];
    input1 int[] = string_to_array('2,3,1,3,4,4,1,5,2,3,1,1,4,5,5,3,5,5,4,1,2,1,1,1,1,1,1,4,1,1,1,4,1,3,1,4,1,1,4,1,3,4,5,1,1,5,3,4,3,4,1,5,1,3,1,1,1,3,5,3,2,3,1,5,2,2,1,1,4,1,1,2,2,2,2,3,2,1,2,5,4,1,1,1,5,5,3,1,3,2,2,2,5,1,5,2,4,1,1,3,3,5,2,3,1,2,1,5,1,4,3,5,2,1,5,3,4,4,5,3,1,2,4,3,4,1,3,1,1,2,5,4,3,5,3,2,1,4,1,4,4,2,3,1,1,2,1,1,3,3,3,1,1,2,2,1,1,1,5,1,5,1,4,5,1,5,2,4,3,1,1,3,2,2,1,4,3,1,1,1,3,3,3,4,5,2,3,3,1,3,1,4,1,1,1,2,5,1,4,1,2,4,5,4,1,5,1,5,5,1,5,5,2,5,5,1,4,5,1,1,3,2,5,5,5,4,3,2,5,4,1,1,2,4,4,1,1,1,3,2,1,1,2,1,2,2,3,4,5,4,1,4,5,1,1,5,5,1,4,1,4,4,1,5,3,1,4,3,5,3,1,3,1,4,2,4,5,1,4,1,2,4,1,2,5,1,1,5,1,1,3,1,1,2,3,4,2,4,3,1', ',')::int[];
    mem int[][] = array_fill(0, array[9, 300]);
begin
    raise notice '%', (select sum(calc_fish(x, 80, mem)) as res from unnest(input) as x);
end $$;
