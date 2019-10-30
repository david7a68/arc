module arc.syntax.flat_ast;

struct FlatAstNode {
    import arc.syntax.ast: AstNode;
    import arc.syntax.location: Span;
    import arc.hash: Key;

    AstNode.Type type;
    Span span;
    uint next;
    union {
        Key key;
        ulong value;
    }
}

pragma(msg, FlatAstNode.sizeof);

bool has_children(FlatAstNode[] nodes, uint index) {
    return nodes[index].next == index;
}

import arc.syntax.ast: AstNode;
FlatAstNode[] flatten(uint count, AstNode* root) {
    auto buffer = new FlatAstNode[](count);
    uint index = 0;

    flatten_node(root, buffer, index);

    return buffer;
}

void flatten_node(AstNode* root, FlatAstNode[] buffer, ref uint index) {
    switch (root.type) with (AstNode.Type) {
        case Invalid:
        case None:
            buffer[index] = FlatAstNode(root.type, root.span, index);
            index++;
            break;
        case Name:
        case Char:
        case Label:
            buffer[index] = FlatAstNode(root.type, root.span, index);
            buffer[index].key = root.key;
            index++;
            break;
        case Integer:
            buffer[index] = FlatAstNode(Integer, root.span, index);
            buffer[index].value = root.value;
            index++;
            break;
        case List:
            buffer[index] = FlatAstNode(List, root.span, index);
            index++;
            if (root.children.length > 0) {
                uint last_child_index;
                foreach (child; root.children) {
                    last_child_index = index;
                    if (child.type == AstNode.ListMember) {
                        buffer[index] = FlatAstNode(AstNode.ListMember, child.span, index);
                        index++;
                        flatten_3(child.children, buffer, index);
                    }
                    else {
                        buffer[index] = FlatAstNode(AstNode.ListRepeat, child.span, index);
                        index++;
                        flatten_2(child.children, buffer, index);
                    }
                    buffer[last_child_index].next = index;
                }
                buffer[last_child_index].next = last_child_index;
            }
            break;
        case Block:
            buffer[index] = FlatAstNode(Block, root.span, index);
            index++;
            if (root.children.length > 0) {
                uint last_child_index;
                foreach (child; root.children) {
                    last_child_index = index;
                    flatten_node_set_next(child, buffer, index);
                    buffer[last_child_index].next = index;
                }
                buffer[last_child_index].next =last_child_index;
            }
            break;
        case Negate:
        case Pointer:
        case GetRef:
            buffer[index] = FlatAstNode(root.type, root.span, index);
            index++;
            flatten_node(root.children[0], buffer, index);
            break;
        case Function:
        case Assign:
        case Less:
        case LessEqual:
        case Greater:
        case GreaterEqual:
        case Equal:
        case NotEqual:
        case And:
        case Or:
        case Add:
        case Subtract:
        case Multiply:
        case Divide:
        case Power:
        case Call:
        case Path:
            buffer[index] = FlatAstNode(root.type, root.span, index);
            index++;
            flatten_2(root.children, buffer, index);
            break;
        case VarExpression:
        case Define:
            buffer[index] = FlatAstNode(root.type, root.span, index);
            index++;
            buffer[index] = FlatAstNode(Name, root.children[0].span, index + 1);
            index++;
            flatten_2(root.children[1 .. $], buffer, index);
            break;
        case If:
            buffer[index] = FlatAstNode(If, root.span, index);
            index++;
            flatten_3(root.children, buffer, index);
            break;
        case Loop:
            buffer[index] = FlatAstNode(Loop, root.span, index);
            index++;
            flatten_node(root.children[0], buffer, index);
            break;
        case Break:
        case Return:
            buffer[index] = FlatAstNode(root.type, root.span, index);
            index++;
            flatten_2(root.children, buffer, index);
            break;
        case Continue:
            buffer[index] = FlatAstNode(Continue, root.span, index);
            index++;
            flatten_node(root.children[0], buffer, index);
            break;
        case Labeled:
            buffer[index] = FlatAstNode(Labeled, root.span, index);
            index++;
            flatten_node(root.children[0], buffer, index);
            break;
        default:
            assert(false, "unfinished");
    }
}

void flatten_node_set_next(AstNode* root, FlatAstNode[] buffer, ref uint index) {
    const saved_index = index;
    flatten_node(root, buffer, index);
    buffer[saved_index].next = index;
}

void flatten_2(AstNode*[] nodes, FlatAstNode[] buffer, ref uint index)
        in (nodes.length == 2) {
    flatten_node_set_next(nodes[0], buffer, index);
    flatten_node(nodes[1], buffer, index);
}

void flatten_3(AstNode*[] nodes, FlatAstNode[] buffer, ref uint index)
        in (nodes.length == 3) {
    flatten_node_set_next(nodes[0], buffer, index);
    flatten_2(nodes[1..$], buffer, index);
}
