using System.Runtime.CompilerServices;
using Esprima.Ast.Adhoc;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Declaration) })]
public sealed partial class ImportExpression : Expression
{
    public ImportExpression(ImportDeclaration source) : base(Nodes.ImportExpression)
    {
        Declaration = source;
    }

    public ImportDeclaration Declaration { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ImportExpression Rewrite(ImportDeclaration source)
    {
        return new ImportExpression(source);
    }
}
