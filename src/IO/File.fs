module FsharpMyExtension.File
open System.IO

/// * Если на диске существует `dir + fileName`, тогда `dir + changeFileName fileName 0`
/// * Если и он существует, тогда `dir + changeFileName fileName 1`
/// * ...
///
/// возвращает полный путь
let getUniqFile dir fileName =
    until (snd >> File.Exists >> not)
        (fun (i, _) ->
            let path =
                let name = Path.changeFileNameWithoutExt (flip (+) (string i)) fileName
                Path.Combine(dir, name)
            i + 1, path
            )
        (0, Path.Combine(dir, fileName))
    |> snd

let move src dst = File.Move(src,dst)
let copy src dst = File.Copy(src,dst)
let writeAllText path cont = File.WriteAllText(path, cont)
let writeAllSeq path (cont: _ seq) = File.WriteAllLines(path, cont)

let uniq f (dir, fileName) =
    let x = getUniqFile dir fileName
    f x; x
let moveUniqS src (dir, fileName) =
    uniq (uncurry File.Move src) (dir, fileName)
let copyUniqS src (dir, fileName) =
    uniq (uncurry File.Copy src) (dir, fileName)
let writeAllTextUniqS (dir, fileName) cont =
    uniq (fun path -> File.WriteAllText(path, cont)) (dir, fileName)

let writeAllSeqUniqS (dir, fileName) (cont:_ seq) =
    uniq (fun path -> File.WriteAllLines(path, cont)) (dir, fileName)

/// Делит путь на папку и файл
let splitOnDirAndFilename = on Path.GetDirectoryName Path.GetFileName
let moveUniq src = splitOnDirAndFilename >> moveUniqS src
let copyUniq src = splitOnDirAndFilename >> copyUniqS src
let writeAllTextUniq path = writeAllTextUniqS (splitOnDirAndFilename path)
let writeAllSeqUniq path = writeAllSeqUniqS (splitOnDirAndFilename path)

