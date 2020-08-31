def matches_one := (type: TokenType, types: TokenType[]) => {
    i := 0; loop {
        # syntax sugar idea, if ... return/break/continue doesn't need braces
        # if i == types.length return false;

        if i == types.length {return false;}
        if type == types[i] {return true;}
        i = i + 1;
    }
};
