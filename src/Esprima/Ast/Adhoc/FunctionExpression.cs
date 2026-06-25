using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using Esprima.Utils;

namespace Esprima.Ast.Adhoc;

[VisitableNode(ChildProperties = new[] { nameof(Id), nameof(Params), nameof(Body) })]
public sealed partial class FunctionExpression : Expression, IFunction
{
    private readonly NodeList<Expression> _parameters;

    public FunctionExpression(
        Identifier? id,
        in NodeList<Expression> parameters,
        BlockStatement body,
        bool generator,
        bool strict,
        bool async) :
        base(Nodes.FunctionExpression)
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

    public bool IsMethodExpression { get; set; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private FunctionExpression Rewrite(Identifier? id, in NodeList<Expression> parameters, BlockStatement body)
    {
        return new FunctionExpression(id, parameters, body, Generator, Strict, Async);
    }
}
