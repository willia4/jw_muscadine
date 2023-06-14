module ElectricLemur.Muscadine.Site.ImagePaths

type ImagePaths = {
    Original: string
    Size1024: string
    Size512: string
    Size256: string
    Size128: string
    Size64: string
}

let choose64 i = i.Size64
let choose128 i = i.Size128
let choose256 i = i.Size256
let choose512 i = i.Size512
let choose1024 i = i.Size1024
let chooseOriginal i = i.Original