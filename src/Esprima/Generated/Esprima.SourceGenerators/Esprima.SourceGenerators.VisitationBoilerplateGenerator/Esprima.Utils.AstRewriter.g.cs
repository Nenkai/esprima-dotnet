#nullable enable

namespace Esprima.Utils;

partial class AstRewriter
{
    protected internal override object? VisitAccessorProperty(Esprima.Ast.AccessorProperty accessorProperty)
    {
        VisitAndConvert(accessorProperty.Decorators, out var decorators);

        var key = VisitAndConvert(accessorProperty.Key);

        var value = VisitAndConvert(accessorProperty.Value, allowNull: true);

        return accessorProperty.UpdateWith(decorators, key, value);
    }

    protected internal override object? VisitArrayExpression(Esprima.Ast.ArrayExpression arrayExpression)
    {
        VisitAndConvert(arrayExpression.Elements, out var elements, allowNullElement: true);

        return arrayExpression.UpdateWith(elements);
    }

    protected internal override object? VisitArrayPattern(Esprima.Ast.ArrayPattern arrayPattern)
    {
        VisitAndConvert(arrayPattern.Elements, out var elements, allowNullElement: true);

        return arrayPattern.UpdateWith(elements);
    }

    protected internal override object? VisitAssignmentExpression(Esprima.Ast.AssignmentExpression assignmentExpression)
    {
        var left = VisitAndConvert(assignmentExpression.Left);

        var right = VisitAndConvert(assignmentExpression.Right);

        return assignmentExpression.UpdateWith(left, right);
    }

    protected internal override object? VisitAssignmentPattern(Esprima.Ast.AssignmentPattern assignmentPattern)
    {
        var left = VisitAndConvert(assignmentPattern.Left);

        var right = VisitAndConvert(assignmentPattern.Right);

        return assignmentPattern.UpdateWith(left, right);
    }

    protected internal override object? VisitAttributeDeclaration(Esprima.Ast.AttributeDeclaration attributeDeclaration)
    {
        var varExpression = VisitAndConvert(attributeDeclaration.VarExpression);

        return attributeDeclaration.UpdateWith(varExpression);
    }

    protected internal override object? VisitAwaitExpression(Esprima.Ast.AwaitExpression awaitExpression)
    {
        var argument = VisitAndConvert(awaitExpression.Argument);

        return awaitExpression.UpdateWith(argument);
    }

    protected internal override object? VisitBinaryExpression(Esprima.Ast.BinaryExpression binaryExpression)
    {
        var left = VisitAndConvert(binaryExpression.Left);

        var right = VisitAndConvert(binaryExpression.Right);

        return binaryExpression.UpdateWith(left, right);
    }

    protected internal override object? VisitBlockStatement(Esprima.Ast.BlockStatement blockStatement)
    {
        VisitAndConvert(blockStatement.Body, out var body);

        return blockStatement.UpdateWith(body);
    }

    protected internal override object? VisitBreakStatement(Esprima.Ast.BreakStatement breakStatement)
    {
        var label = VisitAndConvert(breakStatement.Label, allowNull: true);

        return breakStatement.UpdateWith(label);
    }

    protected internal override object? VisitCallExpression(Esprima.Ast.CallExpression callExpression)
    {
        var callee = VisitAndConvert(callExpression.Callee);

        VisitAndConvert(callExpression.Arguments, out var arguments);

        return callExpression.UpdateWith(callee, arguments);
    }

    protected internal override object? VisitCatchClause(Esprima.Ast.CatchClause catchClause)
    {
        var param = VisitAndConvert(catchClause.Param, allowNull: true);

        var body = VisitAndConvert(catchClause.Body);

        return catchClause.UpdateWith(param, body);
    }

    protected internal override object? VisitChainExpression(Esprima.Ast.ChainExpression chainExpression)
    {
        var expression = VisitAndConvert(chainExpression.Expression);

        return chainExpression.UpdateWith(expression);
    }

    protected internal override object? VisitClassBody(Esprima.Ast.ClassBody classBody)
    {
        VisitAndConvert(classBody.Body, out var body);

        return classBody.UpdateWith(body);
    }

    protected internal override object? VisitClassDeclaration(Esprima.Ast.ClassDeclaration classDeclaration)
    {
        var id = VisitAndConvert(classDeclaration.Id, allowNull: true);

        var superClass = VisitAndConvert(classDeclaration.SuperClass, allowNull: true);

        var body = VisitAndConvert(classDeclaration.Body);

        return classDeclaration.UpdateWith(id, superClass, body);
    }

    protected internal override object? VisitClassExpression(Esprima.Ast.ClassExpression classExpression)
    {
        VisitAndConvert(classExpression.Decorators, out var decorators);

        var id = VisitAndConvert(classExpression.Id, allowNull: true);

        var superClass = VisitAndConvert(classExpression.SuperClass, allowNull: true);

        var body = VisitAndConvert(classExpression.Body);

        return classExpression.UpdateWith(decorators, id, superClass, body);
    }

    protected internal override object? VisitConditionalExpression(Esprima.Ast.ConditionalExpression conditionalExpression)
    {
        var test = VisitAndConvert(conditionalExpression.Test);

        var consequent = VisitAndConvert(conditionalExpression.Consequent);

        var alternate = VisitAndConvert(conditionalExpression.Alternate);

        return conditionalExpression.UpdateWith(test, consequent, alternate);
    }

    protected internal override object? VisitContinueStatement(Esprima.Ast.ContinueStatement continueStatement)
    {
        var label = VisitAndConvert(continueStatement.Label, allowNull: true);

        return continueStatement.UpdateWith(label);
    }

    protected internal override object? VisitDecorator(Esprima.Ast.Decorator decorator)
    {
        var expression = VisitAndConvert(decorator.Expression);

        return decorator.UpdateWith(expression);
    }

    protected internal override object? VisitDelegateDeclaration(Esprima.Ast.DelegateDeclaration delegateDeclaration)
    {
        var identifier = VisitAndConvert(delegateDeclaration.Identifier);

        return delegateDeclaration.UpdateWith(identifier);
    }

    protected internal override object? VisitDoWhileStatement(Esprima.Ast.DoWhileStatement doWhileStatement)
    {
        var body = VisitAndConvert(doWhileStatement.Body);

        var test = VisitAndConvert(doWhileStatement.Test);

        return doWhileStatement.UpdateWith(body, test);
    }

    protected internal override object? VisitExpressionStatement(Esprima.Ast.ExpressionStatement expressionStatement)
    {
        var expression = VisitAndConvert(expressionStatement.Expression);

        return expressionStatement.UpdateWith(expression);
    }

    protected internal override object? VisitFinalizerStatement(Esprima.Ast.FinalizerStatement finalizerStatement)
    {
        var body = VisitAndConvert(finalizerStatement.Body);

        return finalizerStatement.UpdateWith(body);
    }

    protected internal override object? VisitForeachStatement(Esprima.Ast.ForeachStatement foreachStatement)
    {
        var left = VisitAndConvert(foreachStatement.Left);

        var right = VisitAndConvert(foreachStatement.Right);

        var body = VisitAndConvert(foreachStatement.Body);

        return foreachStatement.UpdateWith(left, right, body);
    }

    protected internal override object? VisitForStatement(Esprima.Ast.ForStatement forStatement)
    {
        var init = VisitAndConvert(forStatement.Init, allowNull: true);

        var test = VisitAndConvert(forStatement.Test, allowNull: true);

        var update = VisitAndConvert(forStatement.Update, allowNull: true);

        var body = VisitAndConvert(forStatement.Body);

        return forStatement.UpdateWith(init, test, update, body);
    }

    protected internal override object? VisitFunctionDeclaration(Esprima.Ast.FunctionDeclaration functionDeclaration)
    {
        var id = VisitAndConvert(functionDeclaration.Id, allowNull: true);

        VisitAndConvert(functionDeclaration.Params, out var @params);

        var body = VisitAndConvert(functionDeclaration.Body);

        return functionDeclaration.UpdateWith(id, @params, body);
    }

    protected internal override object? VisitFunctionExpression(Esprima.Ast.Adhoc.FunctionExpression functionExpression)
    {
        var id = VisitAndConvert(functionExpression.Id, allowNull: true);

        VisitAndConvert(functionExpression.Params, out var @params);

        var body = VisitAndConvert(functionExpression.Body);

        return functionExpression.UpdateWith(id, @params, body);
    }

    protected internal override object? VisitIfStatement(Esprima.Ast.IfStatement ifStatement)
    {
        var test = VisitAndConvert(ifStatement.Test);

        var consequent = VisitAndConvert(ifStatement.Consequent);

        var alternate = VisitAndConvert(ifStatement.Alternate, allowNull: true);

        return ifStatement.UpdateWith(test, consequent, alternate);
    }

    protected internal override object? VisitImportAttribute(Esprima.Ast.ImportAttribute importAttribute)
    {
        var key = VisitAndConvert(importAttribute.Key);

        var value = VisitAndConvert(importAttribute.Value);

        return importAttribute.UpdateWith(key, value);
    }

    protected internal override object? VisitImportDeclaration(Esprima.Ast.Adhoc.ImportDeclaration importDeclaration)
    {
        VisitAndConvert(importDeclaration.Specifiers, out var specifiers);

        var target = VisitAndConvert(importDeclaration.Target);

        var alias = VisitAndConvert(importDeclaration.Alias, allowNull: true);

        return importDeclaration.UpdateWith(specifiers, target, alias);
    }

    protected internal override object? VisitImportDefaultSpecifier(Esprima.Ast.ImportDefaultSpecifier importDefaultSpecifier)
    {
        var local = VisitAndConvert(importDefaultSpecifier.Local);

        return importDefaultSpecifier.UpdateWith(local);
    }

    protected internal override object? VisitImportExpression(Esprima.Ast.ImportExpression importExpression)
    {
        var declaration = VisitAndConvert(importExpression.Declaration);

        return importExpression.UpdateWith(declaration);
    }

    protected internal override object? VisitImportNamespaceSpecifier(Esprima.Ast.ImportNamespaceSpecifier importNamespaceSpecifier)
    {
        var local = VisitAndConvert(importNamespaceSpecifier.Local);

        return importNamespaceSpecifier.UpdateWith(local);
    }

    protected internal override object? VisitLabeledStatement(Esprima.Ast.LabeledStatement labeledStatement)
    {
        var label = VisitAndConvert(labeledStatement.Label);

        var body = VisitAndConvert(labeledStatement.Body);

        return labeledStatement.UpdateWith(label, body);
    }

    protected internal override object? VisitListAssignementExpression(Esprima.Ast.ListAssignementExpression listAssignementExpression)
    {
        VisitAndConvert(listAssignementExpression.Declarations, out var declarations);

        return listAssignementExpression.UpdateWith(declarations);
    }

    protected internal override object? VisitListAssignmentStatement(Esprima.Ast.ListAssignmentStatement listAssignmentStatement)
    {
        var left = VisitAndConvert(listAssignmentStatement.Left);

        var right = VisitAndConvert(listAssignmentStatement.Right);

        return listAssignmentStatement.UpdateWith(left, right);
    }

    protected internal override object? VisitMapElement(Esprima.Ast.MapElement mapElement)
    {
        var key = VisitAndConvert(mapElement.Key);

        var value = VisitAndConvert(mapElement.Value);

        return mapElement.UpdateWith(key, value);
    }

    protected internal override object? VisitMapExpression(Esprima.Ast.MapExpression mapExpression)
    {
        VisitAndConvert(mapExpression.Elements, out var elements);

        return mapExpression.UpdateWith(elements);
    }

    protected internal override object? VisitMemberExpression(Esprima.Ast.MemberExpression memberExpression)
    {
        var @object = VisitAndConvert(memberExpression.Object);

        var property = VisitAndConvert(memberExpression.Property);

        return memberExpression.UpdateWith(@object, property);
    }

    protected internal override object? VisitMetaProperty(Esprima.Ast.MetaProperty metaProperty)
    {
        var meta = VisitAndConvert(metaProperty.Meta);

        var property = VisitAndConvert(metaProperty.Property);

        return metaProperty.UpdateWith(meta, property);
    }

    protected internal override object? VisitMethodDeclaration(Esprima.Ast.MethodDeclaration methodDeclaration)
    {
        var id = VisitAndConvert(methodDeclaration.Id, allowNull: true);

        VisitAndConvert(methodDeclaration.Params, out var @params);

        var body = VisitAndConvert(methodDeclaration.Body);

        return methodDeclaration.UpdateWith(id, @params, body);
    }

    protected internal override object? VisitMethodDefinition(Esprima.Ast.MethodDefinition methodDefinition)
    {
        VisitAndConvert(methodDefinition.Decorators, out var decorators);

        var key = VisitAndConvert(methodDefinition.Key);

        var value = VisitAndConvert(methodDefinition.Value);

        return methodDefinition.UpdateWith(decorators, key, value);
    }

    protected internal override object? VisitMethodExpression(Esprima.Ast.Adhoc.MethodExpression methodExpression)
    {
        var id = VisitAndConvert(methodExpression.Id, allowNull: true);

        VisitAndConvert(methodExpression.Params, out var @params);

        var body = VisitAndConvert(methodExpression.Body);

        return methodExpression.UpdateWith(id, @params, body);
    }

    protected internal override object? VisitModuleConstructorStatement(Esprima.Ast.ModuleConstructorStatement moduleConstructorStatement)
    {
        var id = VisitAndConvert(moduleConstructorStatement.Id);

        var body = VisitAndConvert(moduleConstructorStatement.Body);

        return moduleConstructorStatement.UpdateWith(id, body);
    }

    protected internal override object? VisitModuleDeclaration(Esprima.Ast.ModuleDeclaration moduleDeclaration)
    {
        var id = VisitAndConvert(moduleDeclaration.Id, allowNull: true);

        var body = VisitAndConvert(moduleDeclaration.Body);

        return moduleDeclaration.UpdateWith(id, body);
    }

    protected internal override object? VisitNewExpression(Esprima.Ast.NewExpression newExpression)
    {
        var callee = VisitAndConvert(newExpression.Callee);

        VisitAndConvert(newExpression.Arguments, out var arguments);

        return newExpression.UpdateWith(callee, arguments);
    }

    protected internal override object? VisitObjectExpression(Esprima.Ast.ObjectExpression objectExpression)
    {
        VisitAndConvert(objectExpression.Properties, out var properties);

        return objectExpression.UpdateWith(properties);
    }

    protected internal override object? VisitObjectPattern(Esprima.Ast.ObjectPattern objectPattern)
    {
        VisitAndConvert(objectPattern.Properties, out var properties);

        return objectPattern.UpdateWith(properties);
    }

    protected internal override object? VisitPragmaCurrentModuleStatement(Esprima.Ast.PragmaCurrentModuleStatement pragmaCurrentModuleStatement)
    {
        var path = VisitAndConvert(pragmaCurrentModuleStatement.Path);

        return pragmaCurrentModuleStatement.UpdateWith(path);
    }

    protected internal override object? VisitPragmaDumpStatement(Esprima.Ast.PragmaDumpStatement pragmaDumpStatement)
    {
        var path = VisitAndConvert(pragmaDumpStatement.Path);

        return pragmaDumpStatement.UpdateWith(path);
    }

    protected internal override object? VisitPragmaExecStatement(Esprima.Ast.PragmaExecStatement pragmaExecStatement)
    {
        var statements = VisitAndConvert(pragmaExecStatement.Statements);

        return pragmaExecStatement.UpdateWith(statements);
    }

    protected internal override object? VisitPragmaIncludeStatement(Esprima.Ast.PragmaIncludeStatement pragmaIncludeStatement)
    {
        var path = VisitAndConvert(pragmaIncludeStatement.Path, allowNull: true);

        return pragmaIncludeStatement.UpdateWith(path);
    }

    protected internal override object? VisitPragmaPushStrictStatement(Esprima.Ast.PragmaPushStrictStatement pragmaPushStrictStatement)
    {
        var value = VisitAndConvert(pragmaPushStrictStatement.Value, allowNull: true);

        return pragmaPushStrictStatement.UpdateWith(value);
    }

    protected internal override object? VisitPragmaVarStatement(Esprima.Ast.PragmaVarStatement pragmaVarStatement)
    {
        var varType = VisitAndConvert(pragmaVarStatement.VarType);

        var declaration = VisitAndConvert(pragmaVarStatement.Declaration);

        return pragmaVarStatement.UpdateWith(varType, declaration);
    }

    protected internal override object? VisitPrintStatement(Esprima.Ast.PrintStatement printStatement)
    {
        VisitAndConvert(printStatement.Expressions, out var expressions);

        return printStatement.UpdateWith(expressions);
    }

    protected internal override object? VisitProgram(Esprima.Ast.Program program)
    {
        VisitAndConvert(program.Body, out var body);

        return program.UpdateWith(body);
    }

    protected internal override object? VisitPropertyDefinition(Esprima.Ast.PropertyDefinition propertyDefinition)
    {
        VisitAndConvert(propertyDefinition.Decorators, out var decorators);

        var key = VisitAndConvert(propertyDefinition.Key);

        var value = VisitAndConvert(propertyDefinition.Value, allowNull: true);

        return propertyDefinition.UpdateWith(decorators, key, value);
    }

    protected internal override object? VisitRequireStatement(Esprima.Ast.RequireStatement requireStatement)
    {
        var expression = VisitAndConvert(requireStatement.Expression);

        return requireStatement.UpdateWith(expression);
    }

    protected internal override object? VisitRestElement(Esprima.Ast.RestElement restElement)
    {
        var argument = VisitAndConvert(restElement.Argument);

        return restElement.UpdateWith(argument);
    }

    protected internal override object? VisitReturnStatement(Esprima.Ast.ReturnStatement returnStatement)
    {
        var argument = VisitAndConvert(returnStatement.Argument, allowNull: true);

        return returnStatement.UpdateWith(argument);
    }

    protected internal override object? VisitSelfFinalizerExpression(Esprima.Ast.SelfFinalizerExpression selfFinalizerExpression)
    {
        var body = VisitAndConvert(selfFinalizerExpression.Body);

        return selfFinalizerExpression.UpdateWith(body);
    }

    protected internal override object? VisitSequenceExpression(Esprima.Ast.SequenceExpression sequenceExpression)
    {
        VisitAndConvert(sequenceExpression.Expressions, out var expressions);

        return sequenceExpression.UpdateWith(expressions);
    }

    protected internal override object? VisitSourceFileStatement(Esprima.Ast.SourceFileStatement sourceFileStatement)
    {
        var path = VisitAndConvert(sourceFileStatement.Path);

        return sourceFileStatement.UpdateWith(path);
    }

    protected internal override object? VisitSpreadElement(Esprima.Ast.SpreadElement spreadElement)
    {
        var argument = VisitAndConvert(spreadElement.Argument);

        return spreadElement.UpdateWith(argument);
    }

    protected internal override object? VisitStaticBlock(Esprima.Ast.StaticBlock staticBlock)
    {
        VisitAndConvert(staticBlock.Body, out var body);

        return staticBlock.UpdateWith(body);
    }

    protected internal override object? VisitStaticDeclaration(Esprima.Ast.StaticDeclaration staticDeclaration)
    {
        var declaration = VisitAndConvert(staticDeclaration.Declaration);

        return staticDeclaration.UpdateWith(declaration);
    }

    protected internal override object? VisitStaticIdentifier(Esprima.Ast.StaticIdentifier staticIdentifier)
    {
        var id = VisitAndConvert(staticIdentifier.Id);

        return staticIdentifier.UpdateWith(id);
    }

    protected internal override object? VisitSwitchCase(Esprima.Ast.SwitchCase switchCase)
    {
        var test = VisitAndConvert(switchCase.Test, allowNull: true);

        VisitAndConvert(switchCase.Consequent, out var consequent);

        return switchCase.UpdateWith(test, consequent);
    }

    protected internal override object? VisitSwitchStatement(Esprima.Ast.SwitchStatement switchStatement)
    {
        var discriminant = VisitAndConvert(switchStatement.Discriminant);

        VisitAndConvert(switchStatement.Cases, out var cases);

        return switchStatement.UpdateWith(discriminant, cases);
    }

    protected internal override object? VisitTaggedTemplateExpression(Esprima.Ast.TaggedTemplateExpression taggedTemplateExpression)
    {
        var tag = VisitAndConvert(taggedTemplateExpression.Tag);

        var quasi = VisitAndConvert(taggedTemplateExpression.Quasi);

        return taggedTemplateExpression.UpdateWith(tag, quasi);
    }

    protected internal override object? VisitTemplateLiteral(Esprima.Ast.TemplateLiteral templateLiteral)
    {
        VisitAndConvert(templateLiteral.Quasis, out var quasis);

        VisitAndConvert(templateLiteral.Expressions, out var expressions);

        return templateLiteral.UpdateWith(quasis, expressions);
    }

    protected internal override object? VisitThrowStatement(Esprima.Ast.ThrowStatement throwStatement)
    {
        var argument = VisitAndConvert(throwStatement.Argument);

        return throwStatement.UpdateWith(argument);
    }

    protected internal override object? VisitTryStatement(Esprima.Ast.TryStatement tryStatement)
    {
        var block = VisitAndConvert(tryStatement.Block);

        var handler = VisitAndConvert(tryStatement.Handler, allowNull: true);

        return tryStatement.UpdateWith(block, handler);
    }

    protected internal override object? VisitUnaryExpression(Esprima.Ast.UnaryExpression unaryExpression)
    {
        var argument = VisitAndConvert(unaryExpression.Argument);

        return unaryExpression.UpdateWith(argument);
    }

    protected internal override object? VisitUndefStatement(Esprima.Ast.UndefStatement undefStatement)
    {
        var expression = VisitAndConvert(undefStatement.Expression);

        return undefStatement.UpdateWith(expression);
    }

    protected internal override object? VisitVariableDeclaration(Esprima.Ast.VariableDeclaration variableDeclaration)
    {
        VisitAndConvert(variableDeclaration.Declarations, out var declarations);

        return variableDeclaration.UpdateWith(declarations);
    }

    protected internal override object? VisitVariableDeclarator(Esprima.Ast.VariableDeclarator variableDeclarator)
    {
        var id = VisitAndConvert(variableDeclarator.Id);

        var init = VisitAndConvert(variableDeclarator.Init, allowNull: true);

        return variableDeclarator.UpdateWith(id, init);
    }

    protected internal override object? VisitWhileStatement(Esprima.Ast.WhileStatement whileStatement)
    {
        var test = VisitAndConvert(whileStatement.Test);

        var body = VisitAndConvert(whileStatement.Body);

        return whileStatement.UpdateWith(test, body);
    }

    protected internal override object? VisitWithStatement(Esprima.Ast.WithStatement withStatement)
    {
        var @object = VisitAndConvert(withStatement.Object);

        var body = VisitAndConvert(withStatement.Body);

        return withStatement.UpdateWith(@object, body);
    }

    protected internal override object? VisitYieldExpression(Esprima.Ast.YieldExpression yieldExpression)
    {
        var argument = VisitAndConvert(yieldExpression.Argument, allowNull: true);

        return yieldExpression.UpdateWith(argument);
    }
}
