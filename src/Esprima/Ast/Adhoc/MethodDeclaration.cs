using System.Runtime.CompilerServices;
using Esprima.Ast.Adhoc;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Id), nameof(Params), nameof(Body) })]
public sealed partial class MethodDeclaration : Declaration, IFunction
{
    private readonly NodeList<Expression> _parameters;

    public MethodDeclaration(
        Identifier? id,
        in NodeList<Expression> parameters,
        BlockStatement body,
        bool generator,
        bool strict,
        bool async)
        : base(Nodes.MethodDeclaration)
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
    public BlockStatement Body { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public ref readonly NodeList<Expression> Params { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => ref _parameters; }
    public bool Generator { get; }
    public bool Expression { get; }
    public bool Async { get; }
    public bool Strict { get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private MethodDeclaration Rewrite(Identifier? id, in NodeList<Expression> parameters, BlockStatement body)
    {
        return new MethodDeclaration(id, parameters, body, Generator, Strict, Async);
    }
}
