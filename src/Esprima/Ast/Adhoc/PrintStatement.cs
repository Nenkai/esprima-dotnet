using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

// Adhoc
[VisitableNode(ChildProperties = new[] { nameof(Expressions) })]
public sealed partial class PrintStatement : Statement
{
    private readonly NodeList<Expression> _expressions;

    public PrintStatement(NodeList<Expression> expression)
        : base(Nodes.PrintStatement)
    {
        _expressions = expression;
    }

    public ref readonly NodeList<Expression> Expressions => ref _expressions;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PrintStatement Rewrite(in NodeList<Expression> expressions)
    {
        return new PrintStatement(expressions);
    }
}
