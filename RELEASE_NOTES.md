## 2.0.0-prerelease.8
* remove `Net.ContentType`, `WebCacher`, `WebClientDownloader`, `WebDownloader`, `WebDownloaderParallel`

## 2.0.0-prerelease.7.0
* feat: add `getWidth`, `getHeight`, `toArray` to `Array2D`
* feat: add `Enum.is`, `Enum.contains`
* feat: add `ArrayArray` module with `getWidth`, `getHeight`, `rowExists`, `rowForall`, `columnExists`, `columnForall`, `cropBounds`, `crop`
* breaking: remove `Array2D.trimLeft`, use instead `ArrayArray.trim`
* feat: add `ArrayArray.trim`
* breaking: move `Array2D.trimLeft` to `ArrayArray` module

## 2.0.0-prerelease.6.0
* feat: add `Dictionary.tryGetValue`
* feat: add `EnvironmentExt` with local environment
* remove YamlDotNet
* feat: add `UInt64Ext`
* feat: add `Result.builder`
* feat: add `TaskExt` with `continueWith`, `await`, `awaiti`, `runSync` functions

## 2.0.0-prerelease.5.0
* move `Fuchu` from main dependencies to `Tests` group
* feat: add `Array.genericBinarySearch`
* feat: add `Array.binarySearch`

## 2.0.0-prerelease.4.0
* fix: `Array.split`
* feat: add `FParsecExt.parser`
* breaking: rename `ShowList.shows` to `ShowList.showByToString`
* feat: add `ShowList.shows`
* feat: add `deserialize`
* feat: add `empty`

## 2.0.0-prerelease.3.0
* fable: add `List`

## 2.0.0-prerelease.2.0
* fable: add `FSharpExt` and `Pair`
* fable: add `Tree`
* fable: add `Show`

## 2.0.0-prerelease.1.0
* breaking: Fable version 4 or higher is now required to compile the project in JS
* breaking: remove `Parser` from project. Please use `FuniversalParser`

## 1.13.1
* fix: `List.sepBy`

## 1.13.0
* fable: add `Result`
* feat: add `Array.swap`
* feat: add `Array2D.trimLeft`
* feat: add `Int32.toDigits`, `Int32.ofDigits`
* feat: add `Array.generateRandomNumbersBySum`
* feat: add `Bencode`
* feat: add `Int32.getLength`
* feat: `String.chunkBySize`
* feat: add `Regex.create` , `Regex.getMatches`

## 1.12.0
* feat: add `Result.toOption`
* feat: add `Json.tryDes`
* feat: add `FParsecExt.runParserOnSubstringStart` and `FParsecExt.ParserResult`

## 1.11.0
* feat: add `pipeBackwardBuilder`

## 1.10.1
* fix: build all with `-p:DefineConstants=`

## 1.10.0
* fable: Either<_,_>
* Add RELEASE_NOTES.md
