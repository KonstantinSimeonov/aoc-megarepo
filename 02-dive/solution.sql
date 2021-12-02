drop table if exists cmds;

select
    split_part(x, ' ', 1) as type, split_part(x, ' ', 2)::int as value
into cmds
from regexp_split_to_table(
    'forward 6, down 8, down 5, down 9, forward 2, down 5, down 5, forward 1, forward 7, down 8, up 2, down 4, up 8, down 8, forward 3, forward 4, down 1, forward 5, up 7, down 7, down 8, forward 2, up 3, forward 1, forward 6, forward 9, forward 7, forward 8, forward 2, forward 3, up 2, up 8, down 1, forward 7, down 7, down 2, forward 6, down 1, forward 5, down 3, forward 6, down 7, up 1, up 3, forward 7, forward 6, forward 8, down 4, down 2, up 5, down 2, forward 2, up 5, forward 6, down 3, down 1, down 5, forward 6, up 6, down 7, down 8, down 2, forward 3, down 5, down 4, forward 7, forward 9, up 9, up 8, up 4, forward 8, forward 5, down 4, up 2, forward 9, up 5, down 5, up 9, forward 2, forward 3, down 6, down 8, forward 8, up 5, down 5, forward 7, forward 6, forward 8, up 3, forward 3, forward 1, up 8, down 8, down 2, down 4, up 7, up 2, up 9, up 4, forward 6, down 8, down 1, forward 6, forward 6, down 4, down 2, up 7, down 9, down 9, up 2, up 7, down 4, down 2, forward 1, down 1, up 5, up 5, forward 9, up 3, down 7, forward 7, down 4, down 8, up 1, down 4, down 7, forward 5, up 9, forward 5, forward 1, forward 8, forward 6, forward 5, forward 1, down 4, down 6, forward 5, forward 2, forward 3, down 1, up 2, up 9, forward 4, up 8, down 7, down 8, up 7, down 2, forward 7, up 1, forward 5, forward 1, forward 8, forward 1, up 8, down 6, down 7, forward 2, down 8, down 8, forward 8, up 8, down 6, down 7, down 4, down 7, forward 6, up 3, forward 3, down 2, down 8, down 3, down 9, forward 9, forward 7, down 6, down 4, forward 6, down 2, down 7, up 7, up 8, forward 2, forward 8, down 3, up 2, forward 9, down 2, up 3, down 1, down 1, down 4, down 8, up 2, up 8, forward 2, forward 1, up 1, forward 7, down 8, down 1, down 7, up 3, down 3, forward 8, forward 2, forward 7, down 2, up 9, up 3, up 5, down 4, up 3, forward 4, up 5, down 9, down 9, forward 2, forward 2, down 2, down 8, down 3, down 5, forward 6, down 6, up 5, down 2, down 4, down 9, down 3, forward 7, down 1, forward 1, down 4, up 1, down 9, forward 5, up 2, down 3, forward 8, forward 9, up 9, down 2, forward 8, down 4, down 5, forward 6, forward 5, forward 4, down 6, down 9, down 2, forward 9, down 4, up 8, up 9, up 2, up 5, up 5, forward 9, up 1, forward 6, forward 7, forward 8, forward 9, up 2, forward 3, forward 4, forward 6, forward 9, up 5, up 5, down 3, forward 1, forward 3, forward 2, forward 3, forward 6, forward 7, down 4, down 2, down 1, forward 2, down 5, forward 3, forward 6, down 8, down 9, forward 4, forward 6, down 6, down 6, forward 3, down 6, down 8, down 1, forward 7, forward 9, down 2, down 5, forward 1, forward 3, down 2, forward 1, down 8, down 1, forward 4, down 8, forward 5, forward 1, down 7, down 7, forward 3, forward 1, forward 6, forward 7, forward 5, up 1, forward 2, down 9, forward 3, up 1, forward 2, down 1, down 6, down 3, forward 7, down 5, down 4, down 1, forward 9, forward 9, down 5, forward 7, forward 3, forward 5, down 1, forward 6, down 8, up 2, forward 6, down 3, forward 2, forward 9, forward 4, down 1, down 3, forward 9, forward 3, forward 8, forward 9, up 3, up 1, forward 1, forward 2, down 8, down 9, down 2, down 1, down 3, down 2, forward 9, forward 7, down 5, forward 1, forward 6, forward 3, forward 9, down 2, forward 8, down 5, down 1, forward 5, forward 3, down 6, forward 6, down 8, forward 2, up 5, forward 1, down 2, down 6, forward 9, forward 7, down 1, down 3, down 6, up 3, down 4, forward 8, forward 1, forward 7, down 2, down 5, down 9, forward 6, down 5, forward 5, up 1, down 5, forward 8, up 9, forward 2, down 6, forward 2, forward 7, up 2, down 9, down 7, up 7, down 6, up 5, forward 1, down 8, forward 8, forward 1, forward 7, down 9, down 6, forward 3, down 6, down 1, down 1, down 1, down 3, down 7, down 7, down 3, down 5, forward 4, down 4, forward 7, forward 5, down 9, down 9, forward 7, down 3, down 9, down 4, forward 3, down 7, down 2, forward 2, down 6, forward 9, forward 9, forward 5, up 4, down 7, down 2, up 9, up 4, forward 8, forward 1, down 8, up 5, down 4, down 3, forward 2, down 7, down 2, down 1, down 9, forward 7, forward 7, up 8, up 4, down 3, down 8, forward 6, forward 5, forward 5, forward 5, down 3, down 8, forward 4, forward 7, forward 1, up 3, up 9, down 6, up 4, down 7, forward 8, forward 4, forward 3, up 8, up 3, down 3, forward 6, down 2, forward 7, forward 4, forward 8, down 3, down 9, down 9, down 2, forward 8, up 4, down 3, forward 8, forward 5, forward 7, down 6, up 9, forward 3, down 2, forward 5, forward 2, down 7, forward 6, forward 2, up 9, down 1, down 1, forward 4, up 1, forward 9, down 3, down 4, down 2, forward 3, forward 3, forward 3, up 7, up 8, down 5, forward 1, forward 7, up 9, up 3, down 3, down 8, forward 6, up 5, up 5, forward 4, down 2, down 8, down 1, forward 6, down 3, forward 3, forward 6, forward 1, up 3, up 1, down 5, down 2, down 7, down 1, forward 9, down 4, down 8, forward 9, forward 7, forward 8, down 1, down 2, up 7, down 5, down 2, down 1, up 4, up 8, up 7, down 4, forward 3, down 2, down 2, forward 5, forward 4, down 8, up 4, forward 4, up 1, down 3, down 9, down 9, down 3, up 8, forward 1, forward 6, down 6, down 2, forward 8, down 3, forward 8, forward 2, forward 9, up 3, forward 6, down 5, forward 6, forward 2, up 7, down 9, forward 2, up 2, forward 7, down 1, down 5, down 6, forward 8, down 6, forward 4, forward 1, forward 3, forward 4, up 4, forward 4, down 4, forward 2, forward 5, forward 2, forward 5, down 9, up 2, up 1, down 2, up 4, up 5, forward 2, down 3, down 9, forward 3, down 8, down 9, forward 5, down 3, forward 5, down 3, up 8, forward 7, forward 1, down 2, down 7, forward 3, down 8, forward 9, down 4, down 1, down 7, down 4, up 5, forward 1, down 4, forward 1, forward 8, up 1, up 5, up 2, up 2, down 4, down 7, forward 2, down 8, up 8, down 9, down 3, down 6, down 3, down 1, forward 7, up 8, forward 5, up 5, down 8, down 1, down 8, down 6, down 5, forward 2, up 5, down 6, forward 9, up 6, down 5, down 7, up 9, down 1, forward 4, up 6, forward 2, down 5, down 5, forward 2, up 6, forward 1, down 8, forward 4, up 8, down 3, forward 8, down 8, forward 5, down 6, down 3, forward 1, down 4, down 8, up 1, down 1, down 2, up 9, forward 2, forward 3, down 7, down 2, forward 7, up 8, down 2, down 8, down 9, up 1, down 5, down 5, down 4, down 8, down 9, up 5, forward 2, down 4, down 3, down 2, forward 5, forward 8, down 8, down 1, forward 9, down 5, forward 5, down 2, up 3, up 9, down 1, down 9, forward 7, up 7, forward 3, up 6, forward 8, down 2, down 1, down 7, forward 5, down 8, down 4, forward 7, forward 4, down 6, forward 9, down 3, forward 2, down 3, down 1, down 1, up 1, up 3, down 6, forward 3, up 9, down 4, up 2, down 3, up 1, down 8, down 5, forward 7, forward 2, forward 9, down 8, down 5, down 6, up 3, forward 2, up 8, down 4, forward 7, down 8, down 6, down 4, forward 7, up 9, down 4, forward 2, forward 5, down 3, up 6, up 6, down 2, down 4, forward 8, forward 5, forward 3, forward 5, down 5, down 5, down 6, forward 3, forward 7, forward 1, down 8, down 5, forward 7, up 7, down 9, down 9, down 9, up 6, down 2, down 3, forward 1, up 7, up 8, forward 5, down 1, down 3, down 3, forward 5, down 7, down 1, up 2, down 2, down 3, forward 7, down 9, forward 6, down 5, forward 2, down 5, forward 6, up 3, down 8, up 2, forward 5, forward 1, forward 5, forward 8, forward 6, forward 9, forward 6, up 6, up 5, down 8, down 3, down 5, down 2, forward 9, forward 8, down 1, up 1, up 6, down 6, forward 4, down 3, forward 6, forward 1, up 5, down 6, up 9, down 7, down 2, down 9, down 5, forward 5, up 2, forward 8, down 2, down 8, forward 6, down 4, forward 8, down 7, down 8, down 1, forward 3, down 6, down 9, down 3, forward 3, down 8, forward 8, down 7, forward 6, forward 8, down 8, up 7, down 1, forward 2, forward 3, down 5, up 8, down 3, down 4, down 7, forward 9, forward 7, forward 1, down 3, forward 9, down 8, forward 2, down 2, down 9, down 4, down 3, up 6, up 9, down 3, down 2, forward 5, down 3, down 2, down 8, forward 6, forward 5, up 4, forward 9, forward 8, forward 9, down 2, forward 2, up 6, forward 1, down 5, forward 2, down 8, up 2, up 3, down 3, up 2, up 1, up 5, forward 1, forward 2, down 8, up 3, down 9, forward 7, up 5, down 4, down 4, up 3, forward 2, up 5, down 4, down 4, up 5, forward 8, down 8, down 6, forward 7, down 1, down 3, down 1, forward 3, down 5, down 3, forward 3, up 2, forward 2, down 9, up 8, forward 8, up 8, forward 1, forward 9, forward 3, down 8, down 3, forward 8, forward 4, down 2, forward 2, down 2, down 5, down 7, down 5, forward 8, up 3, forward 1, down 1, forward 3, down 9, forward 2, forward 2, forward 7, down 7, down 2, forward 9, up 5, up 7, forward 8, forward 1, down 7, down 8, down 3, forward 6',
    E', '
) as x;

with
    forward as (select sum(value) from cmds where type = 'forward'),
    up as (select sum(value) from cmds where type = 'up'),
    down as (select sum(value) from cmds where type = 'down')
select f.sum * (d.sum - u.sum) as part1
from forward as f, up as u, down as d;

drop aggregate if exists calc_nav(int, int);
drop function if exists calc_next_nav(submarine_state, int, int);
drop type if exists submarine_state;

create type submarine_state as (
    x int,
    aim int,
    depth int
);

create function calc_next_nav(
    agg submarine_state,
    dx int,
    da int)
returns submarine_state
immutable
language plpgsql
as $$
declare
    new_aim int;
  begin
    new_aim = agg.aim + da;
    return (agg.x + dx, new_aim, agg.depth + new_aim * dx);
  end;
$$;

create aggregate calc_nav (int, int) (
    sfunc = calc_next_nav,
    stype = submarine_state,
    initcond = '(0,0,0)'
);

select (calc_nav).x * (calc_nav).depth as part2 from (
    select
        calc_nav(
            (case when type = 'forward' then value else 0 end),
            (case
                when type = 'up' then -value
                when type = 'down' then value
                else 0
            end)
        )
    from cmds
) as whateverbro;
