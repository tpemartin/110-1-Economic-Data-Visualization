GoogleTools <- function(){
  googleTools <- new.env()
  googleTools$upload_excel_as_googlesheet <- function(sourceFile)
  {
    googledrive::drive_upload(
      media=sourceFile,
      path=googledrive::as_dribble("https://drive.google.com/drive/u/0/folders/1tuGqwOkmh3vyBsC05awaoT2P9AnMoAi5"),
      type="spreadsheet"
    ) -> gd
    gd$drive_resource[[1]]$webViewLink |> browseURL()
    gd
    
    googleTools$readUpload <- function(sheetRange){
      googlesheets4::read_sheet(
        ss=gd$drive_resource[[1]]$webViewLink,
        range=sheetRange
      ) -> gs
      return(gs)
    }
  }
  return(googleTools)
}