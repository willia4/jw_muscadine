async function deleteItemClick(event, button) {
  const idAttribute = button.attributes.getNamedItem("data-id");
  const id = idAttribute ? idAttribute.value : null;

  const nameAttribute = button.attributes.getNamedItem("data-name");
  const name = nameAttribute ? nameAttribute.value : null;

  const urlAttribute = button.attributes.getNamedItem("data-url");
  const url = urlAttribute ? urlAttribute.value : null;

  if (!(!!id && !!name && !!url)) {
    alert(`Invalid page state: could not determine item to delete`);
    return;
  }

  const answer = confirm(`Do you want to delete the item ${name}?`);
  if (!answer) return;

  const response = await fetch(url, {
    method: "DELETE",
    mode: "same-origin",
    cache: "no-cache",
    credentials: "same-origin",
  });

  if (!response || response.status !== 200) {
    alert("Could not delete item; response was unsuccessful");
  }

  location.reload();
}

function configureDragAndDrop() {

    const containers = Array.from(document.getElementsByClassName("image-input"));
    for (const container of containers) {
        container.addEventListener("dragenter", (event) => {
            container.classList.add("dragging");
        });

        container.addEventListener("dragleave", (event) => {
            container.classList.remove("dragging");
        });

        container.addEventListener("drop", (event) => {
            event.preventDefault();

            if (event.dataTransfer.files) {
                for (let k = 0; k < event.dataTransfer.files.length; k++) {
                    const file = event.dataTransfer.files[k];

                    const images = container.getElementsByTagName("img");
                    if (images.length >= 1) {
                        const img = images[0];
                        img.src = URL.createObjectURL(file);
                    }

                    const inputs = container.getElementsByTagName("input");
                    if (inputs.length >= 1) {
                        const input = inputs[0];
                        const dT = new DataTransfer();
                        dT.items.add(event.dataTransfer.files[0]);

                        input.files = dT.files;
                    }
                }
            }

            container.classList.remove("dragging");
        });

        container.addEventListener("dragover", (event) => {
            event.preventDefault();
        });

        
    }
}

function configureTags() {
    function configureTagContainer(container) {
        const listBox = container.getElementsByTagName("select")[0];
        const textBox = container.getElementsByClassName("new-tag-field")[0];
        const button = container.getElementsByClassName("new-tag-button")[0];

        button.addEventListener("click", (event) => {
            event.preventDefault();

            let newTag = textBox.value;
            if (typeof newTag !== "string" || newTag === "") return;
            newTag = newTag.trim();

            const existingTagNodes = Array.from(listBox.childNodes);
            const existingTagNode = existingTagNodes.find(c => c.text === newTag);
            if (existingTagNode) {
                existingTagNode.selected = true;
            } else {
                listBox.add(new Option(newTag, newTag, true, true));
            }

            textBox.value = "";
        });
    }

    const containers = Array.from(document.getElementsByClassName("tags-container"));
    containers.forEach(configureTagContainer)
}

function configureMicroblogs() {
    function configureMicroblogContainer(container) {
        const header = container.appendChild(document.createElement("div"));
        header.classList.add("section-header");
        header.appendChild(document.createElement("span")).appendChild(document.createTextNode("Microblogs"));

        const newMicroblogContainer = container.appendChild(document.createElement("div"));
        newMicroblogContainer.classList.add("new-microblog-container");

        const textBox = newMicroblogContainer.appendChild(document.createElement("textarea"));
        textBox.rows = 5;
        textBox.cols = 30;

        const addButtonContainer = newMicroblogContainer.appendChild(document.createElement("div"));
        addButtonContainer.classList.add("add-button-container");

        const addButton = addButtonContainer.appendChild(document.createElement("button"));
        addButton.appendChild(document.createTextNode("Add"));
        addButton.disabled = true;

        const characterCount =  addButtonContainer.appendChild(document.createElement("span"));
        function setCharacterCount(count) {
            if (characterCount.childNodes.length) {
                characterCount.removeChild(characterCount.childNodes[0]);
            }
            characterCount.appendChild(document.createTextNode("" + count));
        }
        setCharacterCount(0);

        textBox.addEventListener("input", (event) => {
            const v = textBox.value;
            const enableButton = typeof(v) === "string" && v.length > 0;
            addButton.disabled = !enableButton;

            setCharacterCount(!enableButton ? 0 : v.length);
        });

        addButton.addEventListener("click", async (event) => {
            event.preventDefault();
            const postBody = {
                "text": textBox.value
            };
            let url = "/admin/" + pageData.slug +"/" + pageData.id + "/microblog";

            const response = await fetch(url, {
                method: "POST",
                mode: "same-origin",
                cache: "no-cache",
                credentials: "same-origin",
                body: JSON.stringify(postBody)
            });

            const allContainerNodes = Array.from(container.childNodes);
            for(const c of allContainerNodes) { container.removeChild(c); }
            configureMicroblogContainer(container);
        });

        let existingTable = container.appendChild(document.createElement("table"));

        let existingTableHeader = existingTable.appendChild(document.createElement("tr"));
        existingTableHeader.appendChild(document.createElement("th")).appendChild(document.createTextNode("Date"));
        existingTableHeader.appendChild(document.createElement("th")).appendChild(document.createTextNode("Text"));
        existingTableHeader.appendChild(document.createElement("th"));

        function makeTableRow(data) {
            let tr = document.createElement("tr");
    
            const d = new Date(data._dateAdded);
            tr.appendChild(document.createElement("td")).appendChild(document.createTextNode(d.toLocaleString()));
            tr.appendChild(document.createElement("td")).appendChild(document.createTextNode(data.text));
    
            const deleteButton = document.createElement("button");
            deleteButton.appendChild(document.createTextNode("Delete"));
    
            tr.appendChild(document.createElement("td")).appendChild(deleteButton);
    
            deleteButton.addEventListener("click", async (event) => {
                event.preventDefault();
    
                const answer = confirm("Delete microblog with text \"" + data.text + "\"?");
                if (!answer) return;
    
                const url = "/admin/" + pageData.slug + "/" + pageData.id + "/microblog/" + data.id;
                const response = await fetch(url, {
                    method: "DELETE",
                    mode: "same-origin",
                    cache: "no-cache",
                    credentials: "same-origin"
                });
    
                if (!response || response.status !== 200) {
                    alert("Could not delete microblog; response was unsuccessful");
                    return;
                }
    
                existingTable.removeChild(tr);
            });
    
            return tr;
        }

        let getUrl = "/" + pageData.slug +"/" + pageData.id + "/microblog";
        fetch(getUrl).then(async (response) => {
            if (response.status !== 200) {
                console.error('Got bad response when retrieving microblogs');
                return;
            }

            const data = await response.json();
            for (const row of data.map(makeTableRow)) {
                existingTable.appendChild(row)
            }
        });
    }

    const containers = Array.from(document.getElementsByClassName("microblog-container"));
    containers.forEach(configureMicroblogContainer)
}

(() => {
  if (document.addEventListener) {
      document.addEventListener("DOMContentLoaded", () => {
          configureDragAndDrop();
          configureTags();
          configureMicroblogs();

          const buttons = document.getElementsByClassName("delete-button");
    
          for (let i = 0; i < buttons.length; i++) {
            const b = buttons[i];

            b.addEventListener("click", (event) => {
              deleteItemClick(event, b);
            });
          }
    });
  }
})();
