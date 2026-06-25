#nullable enable

namespace Esprima.Ast;

partial class AccessorProperty
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt2(Decorators, Key, Value);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitAccessorProperty(this);

    public AccessorProperty UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Decorator> decorators, Esprima.Ast.Expression key, Esprima.Ast.Expression? value)
    {
        if (decorators.IsSameAs(Decorators) && ReferenceEquals(key, Key) && ReferenceEquals(value, Value))
        {
            return this;
        }
        
        return Rewrite(decorators, key, value);
    }
}

partial class ArrayExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullable(Elements);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitArrayExpression(this);

    public ArrayExpression UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Expression?> elements)
    {
        if (elements.IsSameAs(Elements))
        {
            return this;
        }
        
        return Rewrite(elements);
    }
}

partial class ArrayPattern
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullable(Elements);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitArrayPattern(this);

    public ArrayPattern UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Node?> elements)
    {
        if (elements.IsSameAs(Elements))
        {
            return this;
        }
        
        return Rewrite(elements);
    }
}

partial class AssignmentExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Left, Right);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitAssignmentExpression(this);

    public AssignmentExpression UpdateWith(Esprima.Ast.Expression left, Esprima.Ast.Expression right)
    {
        if (ReferenceEquals(left, Left) && ReferenceEquals(right, Right))
        {
            return this;
        }
        
        return Rewrite(left, right);
    }
}

partial class AssignmentPattern
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Left, Right);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitAssignmentPattern(this);

    public AssignmentPattern UpdateWith(Esprima.Ast.Node left, Esprima.Ast.Expression right)
    {
        if (ReferenceEquals(left, Left) && ReferenceEquals(right, Right))
        {
            return this;
        }
        
        return Rewrite(left, right);
    }
}

partial class AttributeDeclaration
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(VarExpression);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitAttributeDeclaration(this);

    public AttributeDeclaration UpdateWith(Esprima.Ast.Expression varExpression)
    {
        if (ReferenceEquals(varExpression, VarExpression))
        {
            return this;
        }
        
        return Rewrite(varExpression);
    }
}

partial class AwaitExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Argument);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitAwaitExpression(this);

    public AwaitExpression UpdateWith(Esprima.Ast.Expression argument)
    {
        if (ReferenceEquals(argument, Argument))
        {
            return this;
        }
        
        return Rewrite(argument);
    }
}

partial class BinaryExpression
{
    internal sealed override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Left, Right);

    protected internal sealed override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitBinaryExpression(this);

    public BinaryExpression UpdateWith(Esprima.Ast.Expression left, Esprima.Ast.Expression right)
    {
        if (ReferenceEquals(left, Left) && ReferenceEquals(right, Right))
        {
            return this;
        }
        
        return Rewrite(left, right);
    }
}

partial class BlockStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitBlockStatement(this);

    public BlockStatement UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Statement> body)
    {
        if (body.IsSameAs(Body))
        {
            return this;
        }
        
        return Rewrite(body);
    }
}

partial class BreakStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullable(Label);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitBreakStatement(this);

    public BreakStatement UpdateWith(Esprima.Ast.Identifier? label)
    {
        if (ReferenceEquals(label, Label))
        {
            return this;
        }
        
        return Rewrite(label);
    }
}

partial class CallExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Callee, Arguments);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitCallExpression(this);

    public CallExpression UpdateWith(Esprima.Ast.Expression callee, in Esprima.Ast.NodeList<Esprima.Ast.Expression> arguments)
    {
        if (ReferenceEquals(callee, Callee) && arguments.IsSameAs(Arguments))
        {
            return this;
        }
        
        return Rewrite(callee, arguments);
    }
}

partial class CatchClause
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt0(Param, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitCatchClause(this);

    public CatchClause UpdateWith(Esprima.Ast.Node? param, Esprima.Ast.BlockStatement body)
    {
        if (ReferenceEquals(param, Param) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(param, body);
    }
}

partial class ChainExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Expression);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitChainExpression(this);

    public ChainExpression UpdateWith(Esprima.Ast.Expression expression)
    {
        if (ReferenceEquals(expression, Expression))
        {
            return this;
        }
        
        return Rewrite(expression);
    }
}

partial class ClassBody
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitClassBody(this);

    public ClassBody UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.ClassElement> body)
    {
        if (body.IsSameAs(Body))
        {
            return this;
        }
        
        return Rewrite(body);
    }
}

partial class ClassDeclaration
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt0_1(Id, SuperClass, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitClassDeclaration(this);

    public ClassDeclaration UpdateWith(Esprima.Ast.Expression? id, Esprima.Ast.Expression? superClass, Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(id, Id) && ReferenceEquals(superClass, SuperClass) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(id, superClass, body);
    }
}

partial class ClassExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt1_2(Decorators, Id, SuperClass, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitClassExpression(this);

    public ClassExpression UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Decorator> decorators, Esprima.Ast.Expression? id, Esprima.Ast.Expression? superClass, Esprima.Ast.Statement body)
    {
        if (decorators.IsSameAs(Decorators) && ReferenceEquals(id, Id) && ReferenceEquals(superClass, SuperClass) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(decorators, id, superClass, body);
    }
}

partial class ConditionalExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Test, Consequent, Alternate);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitConditionalExpression(this);

    public ConditionalExpression UpdateWith(Esprima.Ast.Expression test, Esprima.Ast.Expression consequent, Esprima.Ast.Expression alternate)
    {
        if (ReferenceEquals(test, Test) && ReferenceEquals(consequent, Consequent) && ReferenceEquals(alternate, Alternate))
        {
            return this;
        }
        
        return Rewrite(test, consequent, alternate);
    }
}

partial class ContinueStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullable(Label);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitContinueStatement(this);

    public ContinueStatement UpdateWith(Esprima.Ast.Identifier? label)
    {
        if (ReferenceEquals(label, Label))
        {
            return this;
        }
        
        return Rewrite(label);
    }
}

partial class Decorator
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Expression);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitDecorator(this);

    public Decorator UpdateWith(Esprima.Ast.Expression expression)
    {
        if (ReferenceEquals(expression, Expression))
        {
            return this;
        }
        
        return Rewrite(expression);
    }
}

partial class DelegateDeclaration
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Identifier);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitDelegateDeclaration(this);

    public DelegateDeclaration UpdateWith(Esprima.Ast.Identifier identifier)
    {
        if (ReferenceEquals(identifier, Identifier))
        {
            return this;
        }
        
        return Rewrite(identifier);
    }
}

partial class DoWhileStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Body, Test);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitDoWhileStatement(this);

    public DoWhileStatement UpdateWith(Esprima.Ast.Statement body, Esprima.Ast.Expression test)
    {
        if (ReferenceEquals(body, Body) && ReferenceEquals(test, Test))
        {
            return this;
        }
        
        return Rewrite(body, test);
    }
}

partial class EmptyStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitEmptyStatement(this);
}

partial class ErrorExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitErrorExpression(this);
}

partial class ErrorStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitErrorStatement(this);
}

partial class ExpressionStatement
{
    internal sealed override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Expression);

    protected internal sealed override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitExpressionStatement(this);

    public ExpressionStatement UpdateWith(Esprima.Ast.Expression expression)
    {
        if (ReferenceEquals(expression, Expression))
        {
            return this;
        }
        
        return Rewrite(expression);
    }
}

partial class FinalizerStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitFinalizerStatement(this);

    public FinalizerStatement UpdateWith(Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(body);
    }
}

partial class ForeachStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Left, Right, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitForeachStatement(this);

    public ForeachStatement UpdateWith(Esprima.Ast.Node left, Esprima.Ast.Expression right, Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(left, Left) && ReferenceEquals(right, Right) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(left, right, body);
    }
}

partial class ForStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt0_1_2(Init, Test, Update, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitForStatement(this);

    public ForStatement UpdateWith(Esprima.Ast.StatementListItem? init, Esprima.Ast.Expression? test, Esprima.Ast.Expression? update, Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(init, Init) && ReferenceEquals(test, Test) && ReferenceEquals(update, Update) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(init, test, update, body);
    }
}

partial class FunctionDeclaration
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt0(Id, Params, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitFunctionDeclaration(this);

    public FunctionDeclaration UpdateWith(Esprima.Ast.Identifier? id, in Esprima.Ast.NodeList<Esprima.Ast.Expression> @params, Esprima.Ast.BlockStatement body)
    {
        if (ReferenceEquals(id, Id) && @params.IsSameAs(Params) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(id, @params, body);
    }
}

partial class Identifier
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitIdentifier(this);
}

partial class IfStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt2(Test, Consequent, Alternate);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitIfStatement(this);

    public IfStatement UpdateWith(Esprima.Ast.Expression test, Esprima.Ast.Statement consequent, Esprima.Ast.Statement? alternate)
    {
        if (ReferenceEquals(test, Test) && ReferenceEquals(consequent, Consequent) && ReferenceEquals(alternate, Alternate))
        {
            return this;
        }
        
        return Rewrite(test, consequent, alternate);
    }
}

partial class ImportAttribute
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Key, Value);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitImportAttribute(this);

    public ImportAttribute UpdateWith(Esprima.Ast.Expression key, Esprima.Ast.Literal value)
    {
        if (ReferenceEquals(key, Key) && ReferenceEquals(value, Value))
        {
            return this;
        }
        
        return Rewrite(key, value);
    }
}

partial class ImportDefaultSpecifier
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Local);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitImportDefaultSpecifier(this);

    public ImportDefaultSpecifier UpdateWith(Esprima.Ast.Identifier local)
    {
        if (ReferenceEquals(local, Local))
        {
            return this;
        }
        
        return Rewrite(local);
    }
}

partial class ImportExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Declaration);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitImportExpression(this);

    public ImportExpression UpdateWith(Esprima.Ast.Adhoc.ImportDeclaration declaration)
    {
        if (ReferenceEquals(declaration, Declaration))
        {
            return this;
        }
        
        return Rewrite(declaration);
    }
}

partial class ImportNamespaceSpecifier
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Local);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitImportNamespaceSpecifier(this);

    public ImportNamespaceSpecifier UpdateWith(Esprima.Ast.Identifier local)
    {
        if (ReferenceEquals(local, Local))
        {
            return this;
        }
        
        return Rewrite(local);
    }
}

partial class ImportSpecifier
{
    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitImportSpecifier(this);

    public ImportSpecifier UpdateWith(Esprima.Ast.Expression imported, Esprima.Ast.Identifier local)
    {
        if (ReferenceEquals(imported, Imported) && ReferenceEquals(local, Local))
        {
            return this;
        }
        
        return Rewrite(imported, local);
    }
}

partial class LabeledStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Label, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitLabeledStatement(this);

    public LabeledStatement UpdateWith(Esprima.Ast.Identifier label, Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(label, Label) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(label, body);
    }
}

partial class ListAssignementExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Declarations);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitListAssignementExpression(this);

    public ListAssignementExpression UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Node> declarations)
    {
        if (declarations.IsSameAs(Declarations))
        {
            return this;
        }
        
        return Rewrite(declarations);
    }
}

partial class ListAssignmentStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Left, Right);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitListAssignmentStatement(this);

    public ListAssignmentStatement UpdateWith(Esprima.Ast.ListAssignementExpression left, Esprima.Ast.Expression right)
    {
        if (ReferenceEquals(left, Left) && ReferenceEquals(right, Right))
        {
            return this;
        }
        
        return Rewrite(left, right);
    }
}

partial class Literal
{
    internal sealed override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal sealed override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitLiteral(this);
}

partial class MapElement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Key, Value);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitMapElement(this);

    public MapElement UpdateWith(Esprima.Ast.Expression key, Esprima.Ast.Expression value)
    {
        if (ReferenceEquals(key, Key) && ReferenceEquals(value, Value))
        {
            return this;
        }
        
        return Rewrite(key, value);
    }
}

partial class MapExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Elements);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitMapExpression(this);

    public MapExpression UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.MapElement> elements)
    {
        if (elements.IsSameAs(Elements))
        {
            return this;
        }
        
        return Rewrite(elements);
    }
}

partial class MemberExpression
{
    internal sealed override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Object, Property);

    protected internal sealed override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitMemberExpression(this);

    public MemberExpression UpdateWith(Esprima.Ast.Expression @object, Esprima.Ast.Expression property)
    {
        if (ReferenceEquals(@object, Object) && ReferenceEquals(property, Property))
        {
            return this;
        }
        
        return Rewrite(@object, property);
    }
}

partial class MetaProperty
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Meta, Property);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitMetaProperty(this);

    public MetaProperty UpdateWith(Esprima.Ast.Identifier meta, Esprima.Ast.Identifier property)
    {
        if (ReferenceEquals(meta, Meta) && ReferenceEquals(property, Property))
        {
            return this;
        }
        
        return Rewrite(meta, property);
    }
}

partial class MethodDeclaration
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt0(Id, Params, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitMethodDeclaration(this);

    public MethodDeclaration UpdateWith(Esprima.Ast.Identifier? id, in Esprima.Ast.NodeList<Esprima.Ast.Expression> @params, Esprima.Ast.BlockStatement body)
    {
        if (ReferenceEquals(id, Id) && @params.IsSameAs(Params) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(id, @params, body);
    }
}

partial class MethodDefinition
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Decorators, Key, Value);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitMethodDefinition(this);

    public MethodDefinition UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Decorator> decorators, Esprima.Ast.Expression key, Esprima.Ast.Expression value)
    {
        if (decorators.IsSameAs(Decorators) && ReferenceEquals(key, Key) && ReferenceEquals(value, Value))
        {
            return this;
        }
        
        return Rewrite(decorators, key, value);
    }
}

partial class ModuleConstructorStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Id, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitModuleConstructorStatement(this);

    public ModuleConstructorStatement UpdateWith(Esprima.Ast.Expression id, Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(id, Id) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(id, body);
    }
}

partial class ModuleDeclaration
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt0(Id, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitModuleDeclaration(this);

    public ModuleDeclaration UpdateWith(Esprima.Ast.Expression? id, Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(id, Id) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(id, body);
    }
}

partial class NewExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Callee, Arguments);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitNewExpression(this);

    public NewExpression UpdateWith(Esprima.Ast.Expression callee, in Esprima.Ast.NodeList<Esprima.Ast.Expression> arguments)
    {
        if (ReferenceEquals(callee, Callee) && arguments.IsSameAs(Arguments))
        {
            return this;
        }
        
        return Rewrite(callee, arguments);
    }
}

partial class ObjectExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Properties);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitObjectExpression(this);

    public ObjectExpression UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Node> properties)
    {
        if (properties.IsSameAs(Properties))
        {
            return this;
        }
        
        return Rewrite(properties);
    }
}

partial class ObjectPattern
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Properties);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitObjectPattern(this);

    public ObjectPattern UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Node> properties)
    {
        if (properties.IsSameAs(Properties))
        {
            return this;
        }
        
        return Rewrite(properties);
    }
}

partial class PragmaCurrentModuleStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Path);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPragmaCurrentModuleStatement(this);

    public PragmaCurrentModuleStatement UpdateWith(Esprima.Ast.Expression path)
    {
        if (ReferenceEquals(path, Path))
        {
            return this;
        }
        
        return Rewrite(path);
    }
}

partial class PragmaDumpStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Path);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPragmaDumpStatement(this);

    public PragmaDumpStatement UpdateWith(Esprima.Ast.Expression path)
    {
        if (ReferenceEquals(path, Path))
        {
            return this;
        }
        
        return Rewrite(path);
    }
}

partial class PragmaExecStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Statements);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPragmaExecStatement(this);

    public PragmaExecStatement UpdateWith(Esprima.Ast.BlockStatement statements)
    {
        if (ReferenceEquals(statements, Statements))
        {
            return this;
        }
        
        return Rewrite(statements);
    }
}

partial class PragmaIncludeStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullable(Path);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPragmaIncludeStatement(this);

    public PragmaIncludeStatement UpdateWith(Esprima.Ast.Literal? path)
    {
        if (ReferenceEquals(path, Path))
        {
            return this;
        }
        
        return Rewrite(path);
    }
}

partial class PragmaNoStrictStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPragmaNoStrictStatement(this);
}

partial class PragmaPopStrictStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPragmaPopStrictStatement(this);
}

partial class PragmaPushStrictStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullable(Value);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPragmaPushStrictStatement(this);

    public PragmaPushStrictStatement UpdateWith(Esprima.Ast.Literal? value)
    {
        if (ReferenceEquals(value, Value))
        {
            return this;
        }
        
        return Rewrite(value);
    }
}

partial class PragmaUseStrictStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPragmaUseStrictStatement(this);
}

partial class PragmaVarStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(VarType, Declaration);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPragmaVarStatement(this);

    public PragmaVarStatement UpdateWith(Esprima.Ast.Identifier varType, Esprima.Ast.VariableDeclaration declaration)
    {
        if (ReferenceEquals(varType, VarType) && ReferenceEquals(declaration, Declaration))
        {
            return this;
        }
        
        return Rewrite(varType, declaration);
    }
}

partial class PrintStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Expressions);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPrintStatement(this);

    public PrintStatement UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Expression> expressions)
    {
        if (expressions.IsSameAs(Expressions))
        {
            return this;
        }
        
        return Rewrite(expressions);
    }
}

partial class PrivateIdentifier
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPrivateIdentifier(this);
}

partial class Program
{
    internal sealed override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Body);

    protected internal sealed override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitProgram(this);

    public Program UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Statement> body)
    {
        if (body.IsSameAs(Body))
        {
            return this;
        }
        
        return Rewrite(body);
    }
}

partial class Property
{
    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitProperty(this);

    public Property UpdateWith(Esprima.Ast.Expression key, Esprima.Ast.Node value)
    {
        if (ReferenceEquals(key, Key) && ReferenceEquals(value, Value))
        {
            return this;
        }
        
        return Rewrite(key, value);
    }
}

partial class PropertyDefinition
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt2(Decorators, Key, Value);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitPropertyDefinition(this);

    public PropertyDefinition UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Decorator> decorators, Esprima.Ast.Expression key, Esprima.Ast.Expression? value)
    {
        if (decorators.IsSameAs(Decorators) && ReferenceEquals(key, Key) && ReferenceEquals(value, Value))
        {
            return this;
        }
        
        return Rewrite(decorators, key, value);
    }
}

partial class RequireStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Expression);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitRequireStatement(this);

    public RequireStatement UpdateWith(Esprima.Ast.Expression expression)
    {
        if (ReferenceEquals(expression, Expression))
        {
            return this;
        }
        
        return Rewrite(expression);
    }
}

partial class RestElement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Argument);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitRestElement(this);

    public RestElement UpdateWith(Esprima.Ast.Node argument)
    {
        if (ReferenceEquals(argument, Argument))
        {
            return this;
        }
        
        return Rewrite(argument);
    }
}

partial class ReturnStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullable(Argument);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitReturnStatement(this);

    public ReturnStatement UpdateWith(Esprima.Ast.Expression? argument)
    {
        if (ReferenceEquals(argument, Argument))
        {
            return this;
        }
        
        return Rewrite(argument);
    }
}

partial class SelfExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitSelfExpression(this);
}

partial class SelfFinalizerExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitSelfFinalizerExpression(this);

    public SelfFinalizerExpression UpdateWith(Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(body);
    }
}

partial class SequenceExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Expressions);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitSequenceExpression(this);

    public SequenceExpression UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Expression> expressions)
    {
        if (expressions.IsSameAs(Expressions))
        {
            return this;
        }
        
        return Rewrite(expressions);
    }
}

partial class SourceFileStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Path);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitSourceFileStatement(this);

    public SourceFileStatement UpdateWith(Esprima.Ast.Literal path)
    {
        if (ReferenceEquals(path, Path))
        {
            return this;
        }
        
        return Rewrite(path);
    }
}

partial class SpreadElement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Argument);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitSpreadElement(this);

    public SpreadElement UpdateWith(Esprima.Ast.Expression argument)
    {
        if (ReferenceEquals(argument, Argument))
        {
            return this;
        }
        
        return Rewrite(argument);
    }
}

partial class StaticBlock
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitStaticBlock(this);

    public StaticBlock UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.Statement> body)
    {
        if (body.IsSameAs(Body))
        {
            return this;
        }
        
        return Rewrite(body);
    }
}

partial class StaticDeclaration
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Declaration);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitStaticDeclaration(this);

    public StaticDeclaration UpdateWith(Esprima.Ast.VariableDeclarator declaration)
    {
        if (ReferenceEquals(declaration, Declaration))
        {
            return this;
        }
        
        return Rewrite(declaration);
    }
}

partial class StaticIdentifier
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Id);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitStaticIdentifier(this);

    public StaticIdentifier UpdateWith(Esprima.Ast.Identifier id)
    {
        if (ReferenceEquals(id, Id))
        {
            return this;
        }
        
        return Rewrite(id);
    }
}

partial class Super
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitSuper(this);
}

partial class SwitchCase
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt0(Test, Consequent);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitSwitchCase(this);

    public SwitchCase UpdateWith(Esprima.Ast.Expression? test, in Esprima.Ast.NodeList<Esprima.Ast.Statement> consequent)
    {
        if (ReferenceEquals(test, Test) && consequent.IsSameAs(Consequent))
        {
            return this;
        }
        
        return Rewrite(test, consequent);
    }
}

partial class SwitchStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Discriminant, Cases);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitSwitchStatement(this);

    public SwitchStatement UpdateWith(Esprima.Ast.Expression discriminant, in Esprima.Ast.NodeList<Esprima.Ast.SwitchCase> cases)
    {
        if (ReferenceEquals(discriminant, Discriminant) && cases.IsSameAs(Cases))
        {
            return this;
        }
        
        return Rewrite(discriminant, cases);
    }
}

partial class TaggedTemplateExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Tag, Quasi);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitTaggedTemplateExpression(this);

    public TaggedTemplateExpression UpdateWith(Esprima.Ast.Expression tag, Esprima.Ast.TemplateLiteral quasi)
    {
        if (ReferenceEquals(tag, Tag) && ReferenceEquals(quasi, Quasi))
        {
            return this;
        }
        
        return Rewrite(tag, quasi);
    }
}

partial class TemplateElement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitTemplateElement(this);
}

partial class TemplateLiteral
{
    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitTemplateLiteral(this);

    public TemplateLiteral UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.TemplateElement> quasis, in Esprima.Ast.NodeList<Esprima.Ast.Expression> expressions)
    {
        if (quasis.IsSameAs(Quasis) && expressions.IsSameAs(Expressions))
        {
            return this;
        }
        
        return Rewrite(quasis, expressions);
    }
}

partial class ThisExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => null;

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitThisExpression(this);
}

partial class ThrowStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Argument);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitThrowStatement(this);

    public ThrowStatement UpdateWith(Esprima.Ast.Expression argument)
    {
        if (ReferenceEquals(argument, Argument))
        {
            return this;
        }
        
        return Rewrite(argument);
    }
}

partial class TryStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt1(Block, Handler);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitTryStatement(this);

    public TryStatement UpdateWith(Esprima.Ast.BlockStatement block, Esprima.Ast.CatchClause? handler)
    {
        if (ReferenceEquals(block, Block) && ReferenceEquals(handler, Handler))
        {
            return this;
        }
        
        return Rewrite(block, handler);
    }
}

partial class UnaryExpression
{
    internal sealed override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Argument);

    protected internal sealed override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitUnaryExpression(this);

    public UnaryExpression UpdateWith(Esprima.Ast.Expression argument)
    {
        if (ReferenceEquals(argument, Argument))
        {
            return this;
        }
        
        return Rewrite(argument);
    }
}

partial class UndefStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Expression);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitUndefStatement(this);

    public UndefStatement UpdateWith(Esprima.Ast.Expression expression)
    {
        if (ReferenceEquals(expression, Expression))
        {
            return this;
        }
        
        return Rewrite(expression);
    }
}

partial class VariableDeclaration
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Declarations);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitVariableDeclaration(this);

    public VariableDeclaration UpdateWith(in Esprima.Ast.NodeList<Esprima.Ast.VariableDeclarator> declarations)
    {
        if (declarations.IsSameAs(Declarations))
        {
            return this;
        }
        
        return Rewrite(declarations);
    }
}

partial class VariableDeclarator
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullableAt1(Id, Init);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitVariableDeclarator(this);

    public VariableDeclarator UpdateWith(Esprima.Ast.Identifier id, Esprima.Ast.Expression? init)
    {
        if (ReferenceEquals(id, Id) && ReferenceEquals(init, Init))
        {
            return this;
        }
        
        return Rewrite(id, init);
    }
}

partial class WhileStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Test, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitWhileStatement(this);

    public WhileStatement UpdateWith(Esprima.Ast.Expression test, Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(test, Test) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(test, body);
    }
}

partial class WithStatement
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNext(Object, Body);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitWithStatement(this);

    public WithStatement UpdateWith(Esprima.Ast.Expression @object, Esprima.Ast.Statement body)
    {
        if (ReferenceEquals(@object, Object) && ReferenceEquals(body, Body))
        {
            return this;
        }
        
        return Rewrite(@object, body);
    }
}

partial class YieldExpression
{
    internal override Esprima.Ast.Node? NextChildNode(ref Esprima.Ast.ChildNodes.Enumerator enumerator) => enumerator.MoveNextNullable(Argument);

    protected internal override object? Accept(Esprima.Utils.AstVisitor visitor) => visitor.VisitYieldExpression(this);

    public YieldExpression UpdateWith(Esprima.Ast.Expression? argument)
    {
        if (ReferenceEquals(argument, Argument))
        {
            return this;
        }
        
        return Rewrite(argument);
    }
}
