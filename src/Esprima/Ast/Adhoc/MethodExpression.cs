using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using Esprima.Utils;

namespace Esprima.Ast.Adhoc;

[VisitableNode(ChildProperties = new[] { nameof(Id), nameof(Params), nameof(Body) })]
public sealed partial class MethodExpression : Expression, IFunction
{
    private readonly NodeList<Expression> _parameters;

    public MethodExpression(
        Identifier? id,
        in NodeList<Expression> parameters,
        BlockStatement body,
        bool generator,
        bool strict,
        bool async) :
        base(Nodes.MethodExpression)
    {
        Id = id;
        _parameters = parameters;
        Body = body;
        Generator = generator;
        Expression = false;
        Strict = strict;
        Async = async;
    }

    public Identifier? Id { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public ref readonly NodeList<Expression> Params => ref _parameters;
    public BlockStatement Body { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public bool Generator { get; }
    public bool Expression { get; }
    public bool Async { get; }
    public bool Strict { get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private MethodExpression Rewrite(Identifier? id, in NodeList<Expression> parameters, BlockStatement body)
    {
        return new MethodExpression(id, parameters, body, Generator, Strict, Async);
    }
}
