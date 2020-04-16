module arc.semantic.type_analysis;

import arc.ast;
import arc.semantic.type;
import arc.semantic.symbol;

ArcType* get_rough_type(AstNode* type_expr, AstNode* value_expr) {
    if (type_expr.type != AstNode.InferredType) {
        return get_type_expr_type(type_expr);
    }

    return get_expr_type(value_expr);
}

ArcType* get_expr_type(AstNode* expr) {
    switch (expr.type) {
        case AstNode.Function:
            auto func = new ArcType(ArcType.Function);
            
            foreach (param; expr.get_children()[0].get_children()) {
                auto param_t = get_rough_type(param.get_children()[1], param.get_children()[2]);
                param_t.name = param.get_children()[0].get_key();
                func.func_type.parameters ~= param_t;
            }

            func.func_type.result = get_type_expr_type(expr.get_children()[1]);
            
            return func;

        case AstNode.Char:
            return new ArcType(ArcType.Char);

        case AstNode.Integer:
            auto int_type = new ArcType(ArcType.Integer, 0);
            int_type.integer = Integer.constant;
            return int_type;

        case AstNode.List:
            auto list = new ArcType(ArcType.Aggregate);

            foreach (member; expr.get_children()) {
                auto member_t = get_rough_type(member.get_children()[1], member.get_children()[2]);
                list.members ~= member_t;
            }

            return list;

        default:
            auto unknown = new ArcType(ArcType.Unknown);
            unknown.unknown_type = expr;
            return unknown;
    }
}

ArcType* get_type_expr_type(AstNode* type_expr) {
    switch (type_expr.type) {
        case AstNode.Char:
            return new ArcType(ArcType.Char);

        case AstNode.Name:
            return new ArcType(ArcType.Named, type_expr.get_key());

        case AstNode.Integer:
            auto int_type = new ArcType(ArcType.Integer, 0);
            int_type.integer = Integer.constant;
            return int_type;

        case AstNode.TypeList:
            auto list = new ArcType(ArcType.Aggregate);

            // Type name unknown in this context. If this is a def, the name is assigned later.
            // Otherwise, the name will be generated automatically once structural matching has
            // taken place.

            foreach (member; type_expr.get_children()) {
                auto member_t = get_type_expr_type(member.get_children()[1]);
                member_t.name = member.get_children()[0].get_key();
                list.members ~= member_t;
            }

            return list;

        case AstNode.FunctionType:
            auto func = new ArcType(ArcType.Function);

            foreach (param; type_expr.get_children()[0].get_children()) {
                auto param_t = get_type_expr_type(param.get_children()[1]);
                param_t.name = param.get_children()[0].get_key();
                func.func_type.parameters ~= param_t;
            }

            func.func_type.result = get_type_expr_type(type_expr.get_children()[1]);

            return func;

        default:
            auto unknown = new ArcType(ArcType.Unknown);
            unknown.unknown_type = type_expr;
            return unknown;
    }
}
