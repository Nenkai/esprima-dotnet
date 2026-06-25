using System;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(VarType), nameof(Declaration) })]
public sealed partial class PragmaVarStatement : Statement
{
    public PragmaVarStatement(Identifier typeName, VariableDeclaration declaration) : base(Nodes.PragmaVarStatement)
    {
        VarType = typeName;
        Declaration = declaration;
    }

    public Identifier VarType { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public VariableDeclaration Declaration { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PragmaVarStatement Rewrite(Identifier varType, VariableDeclaration varDecl)
    {
        return new PragmaVarStatement(varType, varDecl);
    }
}
