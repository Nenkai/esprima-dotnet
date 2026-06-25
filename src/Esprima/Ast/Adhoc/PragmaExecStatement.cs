using System;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Statements) })]
public sealed partial class PragmaExecStatement : Statement
{
    public PragmaExecStatement(BlockStatement blockStatement) : base(Nodes.PragmaExecStatement)
    {
        Statements = blockStatement;
    }

    public BlockStatement Statements { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PragmaExecStatement Rewrite(BlockStatement statements)
    {
        return new PragmaExecStatement(statements);
    }
}
