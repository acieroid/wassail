function load() {
    const file = document.querySelector("input[type=file]").files[0];
    const reader = new FileReader();
    reader.onload = function() {
        
        console.log(reader.result);
    };
    reader.readAsText(file);
}
