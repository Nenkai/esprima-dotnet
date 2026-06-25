using System;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Path) })]
public sealed partial class PragmaCurrentModuleStatement : Statement
{
    public PragmaCurrentModuleStatement(Expression path) : base(Nodes.PragmaCurrentModuleStatement)
    {
        Path = path;
    }

    public Expression Path { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PragmaCurrentModuleStatement Rewrite(Expression path)
    {
        return new PragmaCurrentModuleStatement(path);
    }
}
