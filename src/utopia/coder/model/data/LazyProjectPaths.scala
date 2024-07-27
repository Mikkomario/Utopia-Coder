package utopia.coder.model.data

import utopia.flow.parse.file.FileExtensions._
import utopia.flow.view.immutable.View

import java.nio.file.Path

/**
 * Contains paths to project-specific directories. Initialized lazily.
 * @author Mikko Hilpinen
 * @since 27.07.2024, v1.1
 */
case class LazyProjectPaths(rootView: View[Path], inputView: View[Path], outputView: View[Path],
                            srcView: View[Seq[Path]])
	extends ProjectPaths
{
	override def root: Path = rootView.value
	override def input: Path = inputView.value
	override def output: Path = outputView.value
	
	override def sources: Seq[Path] = srcView.value
	
	override def under(rootPath: Path): ProjectPaths = LazyProjectPaths(
		rootView.mapValue { rootPath/_ },
		inputView.mapValue { rootPath/_ }, outputView.mapValue { rootPath/_ },
		srcView.mapValue { _.map { rootPath/_ } })
}
