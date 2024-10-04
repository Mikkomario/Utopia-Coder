package utopia.coder.model.data

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Empty
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.FromModelFactoryWithSchema
import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.mutable.DataType.StringType
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.file.FileUtils
import utopia.flow.util.EitherExtensions._

import java.nio.file.Path

object ProjectPaths extends FromModelFactoryWithSchema[ProjectPaths]
{
	// ATTRIBUTES   -------------------------
	
	override val schema = ModelDeclaration("models" -> StringType, "output" -> StringType)
	
	
	// IMPLEMENTED  -------------------------
	
	override protected def fromValidatedModel(model: Model) = {
		val root = model("root").string.map { s => s: Path }
		def path(read: String) = root match {
			case Some(root) => root/read
			case None => read: Path
		}
		val input = path(model("models").getString)
		val output = path(model("output").getString)
		val sources = {
			model("src").string match {
				// Case: Legacy format => Parses src & alt_src separately
				case Some(srcStr) =>
					val src = path(srcStr)
					val altSrc = model("alt_src").string.map(path)
					src +: altSrc.emptyOrSingle
					
				// Case: Current format => Expects a json array of strings
				case None => model("sources").getVector.map { v => path(v.getString) }
			}
		}
		// NB: The .getOrElse is here simply to provide some backwards compatibility. Obviously may cause logic errors.
		apply(root.getOrElse(FileUtils.workingDirectory), input, output, sources)
	}
	
	
	// OTHER    -----------------------------
	
	/**
	 * @param rootDirectory Path to this project's root directory.
	 *                      Typically the other directories are sub-directories of this directory.
	 * @param modelsDirectory Directory where model / project / module / input files are read from
	 * @param outputDirectory Directory where generated files will be placed
	 * @param sources Directories to the project source files / src folders
	 * @return A new set of project paths
	 */
	def apply(rootDirectory: Path, modelsDirectory: Path, outputDirectory: Path, sources: Seq[Path] = Empty): ProjectPaths =
		_ProjectPaths(rootDirectory, modelsDirectory, outputDirectory, sources)
	
	/**
	  * @param root The project directory
	  * @return Project paths within that directory, assuming default setup
	  */
	def apply(root: Path): ProjectPaths = apply(root/"data/models", root/"data/coder-build", root/"src")
	
	
	// NESTED   ----------------------------
	
	case class _ProjectPaths(root: Path, input: Path, output: Path, sources: Seq[Path])
		extends ProjectPaths
	{
		// OTHER    ---------------------------
		
		/**
		 * @param rootPath Path to which all paths specified in this set are relative
		 * @return Copy of these paths where the root path is included in each path
		 */
		override def under(rootPath: Path) =
			_ProjectPaths(rootPath/root, rootPath/input, rootPath/output, sources.map { rootPath/_ })
	}
}

/**
  * Contains references to paths that are used for reading project files
  * @author Mikko Hilpinen
  * @since 29.6.2022, Vault Coder v1.5.1
  */
trait ProjectPaths extends ModelConvertible
{
	// ABSTRACT ---------------------------
	
	/**
	 * @return Path to this project's root directory.
	 *         Typically the other directories are sub-directories of this directory.
	 */
	def root: Path
	
	/**
	 * @return Directory where model / project / module / input files are read from
	 */
	def input: Path
	/**
	 * @return Directory where generated files will be placed
	 */
	def output: Path
	
	/**
	 * @return Directories which hold project source files / package-specific sub-directories.
	 */
	def sources: Seq[Path]
	
	/**
	 * @param rootPath Path to which all paths specified in this set are relative
	 * @return Copy of these paths where the root path is included in each path
	 */
	def under(rootPath: Path): ProjectPaths
	
	
	// COMPUTED ---------------------------
	
	/**
	 * @return Directory to the project source files / src folder
	 */
	@deprecated("Deprecated for removal. Please use .sourceDirectories instead", "v1.1")
	def src: Path = sources.head
	/**
	 * @return Additional source folder, typically used with multi-part modules
	 */
	@deprecated("Deprecated for removal. Please use .sourceDirectories instead", "v1.1")
	def altSrc: Option[Path] = sources.lift(1)
	
	
	// IMPLEMENTED  -----------------------
	
	override def toModel = Model.from(
		"root" -> root.toJson,
		"models" -> input.relativeTo(root).either.toJson,
		"output" -> output.relativeTo(root).either.toJson,
		"sources" -> sources.map { _.relativeTo(root).either.toJson })
}
