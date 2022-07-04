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

    const containers = document.getElementsByClassName("image-input");
    for (let i = 0; i < containers.length; i++) {
        const container = containers[i];

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

    const containers = document.getElementsByClassName("tags-container");
    for (let i = 0; i < containers.length; i++) {
        configureTagContainer(containers[i]);
    }
}

(() => {
  if (document.addEventListener) {
      document.addEventListener("DOMContentLoaded", () => {
          configureDragAndDrop();
          configureTags();

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
