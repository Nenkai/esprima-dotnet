using System.Reflection.Emit;
using System.Runtime.CompilerServices;

namespace Esprima.Ast.Adhoc;

[VisitableNode(ChildProperties = new[] { nameof(Specifiers), nameof(Target), nameof(Alias) })]
public sealed partial class ImportDeclaration : Declaration
{
    private readonly NodeList<ImportDeclarationSpecifier> _specifiers;

    public ImportDeclaration(
        in NodeList<ImportDeclarationSpecifier> namespacePath, Identifier target, Identifier? alias)
            : base(Nodes.ImportDeclaration)
    {
        _specifiers = namespacePath;
        Target = target;
        Alias = alias;
    }

    public ref readonly NodeList<ImportDeclarationSpecifier> Specifiers { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => ref _specifiers; }
    public Identifier Target { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public Identifier? Alias { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ImportDeclaration Rewrite(in NodeList<ImportDeclarationSpecifier> namespacePath, Identifier target, Identifier? alias)
    {
        return new ImportDeclaration(namespacePath, target, alias);
    }
}
