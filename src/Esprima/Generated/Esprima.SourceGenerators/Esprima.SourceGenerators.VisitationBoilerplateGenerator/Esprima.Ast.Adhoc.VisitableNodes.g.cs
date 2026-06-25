#nullable enable

namespace Esprima.Ast.Adhoc;

partial class FunctionExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt0(Id, Params, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitFunctionExpression(this);

    public FunctionExpression UpdateWith(Esprima.Ast.Identifier? id, in Esprima.Ast.NodeList<Esprima.Ast.Expression> @params, Esprima.Ast.BlockStatement body)
    {
        if (ReferenceEquals(id, Id) && @params.IsSameAs(Params) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(id, @params, body);
    }
}

partial class ImportDeclaration
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt2(Specifiers, Target, Alias);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitImportDeclaration(this);

    public ImportDeclaration UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.ImportDeclarationSpecifier> specifiers, Esprima.Ast.Identifier target, Esprima.Ast.Identifier? alias)
    {
        if (specifiers.IsSameAs(Specifiers) && ReferenceEquals(target, Target) && ReferenceEquals(alias, Alias))
        {
            return this;
        }
        
        return Rewrite(specifiers, target, alias);
    }
}

partial class MethodExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt0(Id, Params, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitMethodExpression(this);

    public MethodExpression UpdateWith(Esprima.Ast.Identifier? id, in Esprima.Ast.NodeList<Esprima.Ast.Expression> @params, Esprima.Ast.BlockStatement body)
    {
        if (ReferenceEquals(id, Id) && @params.IsSameAs(Params) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(id, @params, body);
    }
}
