grammar LolSql {
    rule TOP { ^ <statement> $ {*} }

    rule statement {
        'HAI!'
	[
        | <select>
        | <update>
	]
	KTHNXBYE
        {*}
    }

    rule select {
        <from>
        <columns>
        <where>?
        {*}
    }

    rule update {
        <from>
        <set>
        <where>?
        {*}
    }

    rule from {
         I\'M IN UR \`<ident>\` {*}
    }

    rule where {
        I CAN HAZ \`$<column>=<ident>\`
        $<comp>=[ LIEK | '=' | '<' | '<=' | '>' | '>=' ]
        <value>
        {*}
    }

    rule set {
        PLZ MAKES
        $<sets>=[ \`<ident>\` LIEKS <value> ]**','
        {*}
    }

    rule columns {
        SELECTIN UR <column>**',' {*}
    }

    rule column {
        \`$<name>=<ident>\` [AZ \`$<as>=<ident>\`]? {*}
    }

    rule value { <str_value> | <number> }
    rule str_value { \' [ '\\\'' | <-[']>+ ]* \' }
    rule number { \d+[\.\d+]? }
}


class Column {
    has $.name;
    has $.as;
    has $.new_value;
}


class Where {
    has $.column;
    has $.comparative;
    has $.value;

    method to_sql() {
        return ($.column, $.comparative, $.value).join(' ');
    }
}


class Select {
    has $.table_name;
    has Column @.columns;
    has Where $.where;

    method to_sql {
        return [~] gather {
            take 'SELECT ';
            take @.columns.map({
                .name ~ (.as ?? ' AS ' ~ .as !! '')
            }).join(', ');
            take ' FROM ' ~ $.table_name;
            if $.where {
                take ' WHERE ' ~ $.where.to_sql;
            }
            take "\n";
        }
    }
}


class Update {
    has $.table_name;
    has Column @.columns;
    has Where $.where;

    method to_sql {
        return [~] gather {
            take 'UPDATE ' ~ $.table_name ~ ' SET ';
            take @.columns.map({
                .name ~ ' = ' ~ .new_value
            }).join(', ');
            if $.where {
                take ' WHERE ' ~ $.where.to_sql;
            }
            take "\n";
        }
    }
}


class LolSqlToTree {
    method TOP($/) {
        make $<statement>.ast;
    }

    method statement($/) {
        if $<select> {
            make $<select>.ast;
        }
        elsif $<update> {
            make $<update>.ast;
        }
    }

    method select($/) {
        my %optionals;
        if $<where> {
            %optionals<where> = $<where>[0].ast;
        }
        make Select.new(
            table_name => $<from>.ast,
            columns    => $<columns>.ast,
            |%optionals
        );
    }

    method update($/) {
        my %optionals;
        if $<where> {
            %optionals<where> = $<where>[0].ast;
        }
        make Update.new(
            table_name => $<from>.ast,
            columns    => $<set>.ast,
            |%optionals
        );
    }

    method from($/) {
        make ~$<ident>;
    }

    method columns($/) {
        my Column @columns;
        for $<column>.list -> $col {
            push @columns, $col.ast;
        }
        make @columns;
    }

    method column($/) {
        make Column.new(
            name => ~$<name>,
            as => $<as>?? ~$<as> !! undef
        );
    }

    method set($/) {
        my Column @columns;
        for $<set>.list -> $set {
            push @columns, Column.new(
                name      => $<ident>.trim,
                new_value => $<value>.trim
            );
        }
        make @columns;
    }

    method where($/) {
        my $comp = $<comp>.trim;
        if $comp eq 'LIEK' { $comp = 'LIKE' }
        make Where.new(
            column      => $<column>.trim,
            comparative => $comp,
            value       => $<value>.trim
        );
    }
}


loop {
    my $expr = $*IN.get;
    my $match = LolSql.parse($expr,
            :action(LolSqlToTree.new));
    if $match {
        my $tree = $match.ast;
        say $tree.to_sql;
    } else {
        say "PARSE FAIL! UR DOIN IT RONG";
    }
}



