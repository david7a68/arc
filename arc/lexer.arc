
import arc::data::hash;
import arc::data::source;

def TokenType : ubyte;

def matches_one := (type: TokenType, types: TokenType[]) -> {
    i := 0; loop {
        # syntax sugar idea, if ... return/break/continue doesn't need braces
        # if i == types.length return false;

        if i == types.length {return false;}
        if type == types[i] {return true;}
        i = i + 1;
    }
};

def Token : (
    type: TokenType,
    span: Span,
    hash: ulong
);

def TokenBuffer : (
    current_token_index : uint,
    next_buffer_index   : uint,
    span_offset         : uint,
    tokens              : Token[],
    source_text         : char[],
    current             : Token,
    done                : bool
);

def init_tokenbuffer := (buffer: *TokenBuffer, text: char[], span_offset: uint) -> {
    buffer.source_text = text;
    buffer.span_offset = span_offset;
    fill_buffer(buffer);
};

def advance_token := (buffer: *TokenBuffer) -> {
    buffer.current_token_index = buffer.current_token_index + 1;
    if buffer.current_token_index == tokens.length {
        fill_buffer(buffer);
    }
    else {
        buffer.current = buffer.tokens[buffer.current_token_index];
        buffer.done = buffer.current.type == TokDone;
    }
};

def fill_buffer := (buffer: *TokenBuffer) -> {
    base    := buffer.source_text.ptr;
    current := base + buffer.next_buffer_index;
    end     := base + buffer.source_text.length;

    buffer.tokens[0] = scan_token(base, current, end, buffer.span_offset);
    i := 0; loop {
        if (buffer.tokens[i - 1].type == TokDone) {break;}
        buffer.tokens[i] = scan_token(base, current, end buffer.span_offset);
        i = i + 1;
    }

    read := current - (base + buffer.next_buffer_index);
    buffer.next_buffer_index = buffer.next_buffer_index + read;

    buffer.current_token_index = 0;
    buffer.current = buffer.tokens[0];
    buffer.done = buffer.current.type == TokDone;
};

def keyword_map := gen_table(
    Pair(digest "and",      TokAnd),
    Pair(digest "or",       TokOr),
    Pair(digest "not",      TokNot),
    Pair(digest "if",       TokIf),
    Pair(digest "else",     TokElse),
    Pair(digest "loop",     TokLoop),
    Pair(digest "break",    TokBreak),
    Pair(digest "return",   TokReturn),
    Pair(digest "continue", TokContinue),
    Pair(digest "def",      TokDef),
    Pair(digest "import",   TokImport),
);

# scan_token not implement because of lack of pattern matching capability
