module arc.semantic.tests.scopes;

import arc.syntax.ast: AstNode;

struct Node {
    AstNode.Type type;
    Node[] children;
}

/*
    Node(List, [
        Node(ListMember, [
            Node(None),
            Node(None),
            Node(Name)
        ])
    ])
*/