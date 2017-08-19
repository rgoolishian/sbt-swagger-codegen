/* Copyright 2015 UniCredit S.p.A.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package eu.unicredit.swagger.generators

import eu.unicredit.swagger.SwaggerConversion
import treehugger.forest._
import definitions._
import io.swagger.models.{Model, ModelImpl}
import treehuggerDSL._
import io.swagger.parser.SwaggerParser
import io.swagger.models.properties._
import eu.unicredit.swagger.StringUtils._

import scala.collection.JavaConverters._

class DefaultModelGenerator extends ModelGenerator with SwaggerConversion {

  def generateClass(name: String, props: Iterable[(String, Property)], comments: Option[String]): String = {
    val GenClass = RootClass.newClass(name)



    val params: Iterable[ValDef] = props.map{
      case (pname, prop: ObjectProperty) =>
        val tpe = if (!prop.getRequired)
          OptionClass TYPE_OF TYPE_REF(pname)
        else
          TYPE_REF(pname)
        PARAM(handleSpaceInParamName(pname), tpe): ValDef
      case (pname, prop) =>
        PARAM(handleSpaceInParamName(pname), propType(prop)): ValDef
    }

    val tree: Tree =
      if (params.isEmpty)
        OBJECTDEF(GenClass) withFlags Flags.CASE
      else
        CLASSDEF(GenClass) withFlags Flags.CASE withParams params

    val resTree = comments.map(tree withComment _).getOrElse(tree)

    treeToString(resTree)
  }

  def generateModelInit(packageName: String): String = {
    //val initTree =
    //PACKAGE(packageName)

    //treeToString(initTree)
    "package " + packageName
  }

  def getModels(paramName: String, model: Model): Iterable[(String, Model)] = {
    Iterable((paramName, model)) ++ getProperties(model).flatMap {
      case (pname, prop: ObjectProperty) =>
        val newModel = new ModelImpl
        newModel.setName(pname)
        newModel.setProperties(prop.getProperties)
        getModels(pname, newModel)
      case _ =>
        Iterable((paramName, model))
    }
  }

  def generate(fileName: String, destPackage: String): Iterable[SyntaxString] = {
    val swagger = new SwaggerParser().read(fileName)

    val models: Iterable[(String, Model)] = swagger.getDefinitions.asScala.flatMap{
      case (name, model) => getModels(name, model)
    }

    for {
      (name, model) <- models
    } yield
      SyntaxString(name + ".scala",
                   generateModelInit(destPackage),
                   generateClass(name, getProperties(model), Option(model.getDescription)))
  }
}
