module FsharpMyExtension.StringCompression
open System.IO
open System.IO.Compression

let enc = System.Text.Encoding.GetEncoding("iso-8859-1")

let compress (str: string) =
    use outputStream = new MemoryStream()

    // https://stackoverflow.com/questions/34775652/gzipstream-works-when-writing-to-filestream-but-not-memorystream
    do
        use compressor = new GZipStream(outputStream, CompressionLevel.Optimal)

        let bytes = System.Text.UTF8Encoding.UTF8.GetBytes(str)
        use inputStream = new MemoryStream(bytes)

        inputStream.CopyTo(compressor)

    outputStream.ToArray()
    |> enc.GetString

let decompress (compressedString: string) =
    let bytes = enc.GetBytes(compressedString)
    use inputStream = new MemoryStream(bytes)
    use decompressor = new GZipStream(inputStream, CompressionMode.Decompress)

    use outputStream = new MemoryStream()
    decompressor.CopyTo(outputStream)

    outputStream.ToArray()
    |> System.Text.Encoding.UTF8.GetString
