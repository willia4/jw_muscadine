document.addEventListener("DOMContentLoaded", () => {
    function findElementSid(el) {
        if (!el) { return undefined; }
        if (el.dataset && el.dataset.sid) {
            return el.dataset.sid;
        }
        
        return findElementSid(el.parentNode);
    }
    
    function createElementSidMappings(els) {
        return Array.from(els).reduce((acc, el) => {
            const sid = findElementSid(el);
            if (sid) {
                if (acc[sid]) {
                    acc[sid].push(el);
                } else {
                    acc[sid] = [ el ];
                }
            }
            return acc;
        }, {});
    }
    function findAnnotations() {
        const asideSids =  createElementSidMappings(document.getElementsByTagName("aside"));
        const headerSids = createElementSidMappings(document.getElementsByTagName("header"));
        const contentSids = createElementSidMappings(document.querySelectorAll("div.section-content"));
        const sectionRows = createElementSidMappings(document.querySelectorAll("div.section-row"));
        
        const sids = Object.keys(asideSids).filter(s => Object.keys(headerSids).includes(s));
        
        return sids.map(sid => {
            return {
                aside: asideSids[sid][0], // should only ever be one aside or header
                header: headerSids[sid][0],
                content: contentSids[sid] || [],
                sectionRows: sectionRows[sid] || []
            }
        });
    }

    function configureAnnotation(annotation) {
        const headerParent = annotation.header.parentNode;
        if (!headerParent) { return; }

        
        const toggleButton = document.createElement("button");
        toggleButton.classList.add("no-print");
        toggleButton.classList.add("annotation-button");

        const showText = document.createTextNode("<< show annotation >>");
        const hideText = document.createTextNode("<< hide annotation >>");
        
        toggleButton.appendChild(showText);
        toggleButton.dataset.status = "not-shown";
        
        toggleButton.addEventListener("click", () => {
           const buttonState = toggleButton.dataset.status;
           if (buttonState === 'not-shown') {
               toggleButton.removeChild(showText);
               toggleButton.appendChild(hideText);
               toggleButton.dataset.status = 'shown';
               annotation.aside.classList.add('shown');
               
               annotation.header.classList.add('has-annotation');
               annotation.content.forEach(el => el.classList.add('has-annotation'));
               annotation.aside.parentNode.classList.add('has-annotation');
               //annotation.sectionRows.forEach(el => el.classList.add('has-annotation'));
               
               annotation.header.scrollIntoView({
                   behavior: "smooth",
                   block: "start",
                   inline: "nearest"
               });
           }
           else {
               toggleButton.removeChild(hideText);
               toggleButton.appendChild(showText);
               toggleButton.dataset.status = 'not-shown';
               
               annotation.aside.classList.remove('shown');
               annotation.header.classList.remove('has-annotation');
               annotation.content.forEach(el => el.classList.remove('has-annotation'));
               annotation.aside.parentNode.classList.remove('has-annotation');
               //annotation.sectionRows.forEach(el => el.classList.remove('has-annotation'));
           }
        });

        headerParent.appendChild(toggleButton);
    }

    findAnnotations().forEach(configureAnnotation);

});
//
// // document.addEventListener("DOMContentLoaded", () => {
// //    
// //     function findParentSection(el) {
// //         const idAttribute = el.attributes["data-container-id"];
// //         if (idAttribute && idAttribute.value) {
// //             const container = document.getElementById(idAttribute.value);
// //             if (container) {
// //                 return container;
// //             }
// //         }
// //
// //         const parent = el.parentNode;
// //         if (!parent) {
// //             return undefined;
// //         }
// //         if (parent.tagName.toLowerCase() === 'section') {
// //             return parent;
// //         }
// //         return findParentSection(parent);
// //     }
// //
// //     function findSectionHeader(el) {
// //         const children =
// //             (el && el.tagName.toLowerCase() === 'section')
// //             ? Array.from(el.childNodes)
// //             : [];
// //         return children.find(c => c.tagName.toLowerCase() === 'header');
// //     }
// //
// //     function findAnnotations() {
// //         const asides = Array.from(document.getElementsByTagName("aside"));
// //         return asides.map(aside => {
// //                     const section = findParentSection(aside);
// //                     const headerId = aside.attributes["data-container-header-id"];
// //                    
// //                     const header =
// //                         headerId && headerId.value && document.getElementById(headerId.value)
// //                         ? document.getElementById(headerId.value)
// //                         : findSectionHeader(section);
// //                    
// //                     return {
// //                         aside: aside,
// //                         section: section,
// //                         header: header
// //                     }
// //                 })
// //                 .filter(o => !!o.aside && !!o.section && !!o.header);
// //     }    
// //
// //     function configureAnnotation(annotation) {
// //         const toggleButton = document.createElement("button");
// //         toggleButton.classList.add("no-print");
// //         toggleButton.classList.add("annotation-button");
// //        
// //         const showText = document.createTextNode("<< show annotation >>");
// //         const hideText = document.createTextNode("<< hide annotation >>");
// //        
// //         toggleButton.appendChild(showText);
// //         toggleButton.dataset.status = "not-shown";
// //        
// //         toggleButton.addEventListener("click", () => {
// //             const buttonState = toggleButton.dataset.status;
// //            
// //             if (buttonState === 'not-shown') {
// //                 toggleButton.removeChild(showText);
// //                 toggleButton.appendChild(hideText);
// //                 toggleButton.dataset.status = 'shown';
// //                 annotation.section.classList.add("has-annotation");
// //             } else {
// //                 toggleButton.removeChild(hideText);
// //                 toggleButton.appendChild(showText);
// //                 toggleButton.dataset.status = 'not-shown';
// //                 annotation.section.classList.remove("has-annotation");
// //             }
// //         });
// //        
// //         const headerChildren = Array.from(annotation.header.childNodes);
// //         const firstHeaderTextNodeIndex = headerChildren.findIndex(c => c.nodeName === '#text');
// //         const afterFirstHeaderTextNode = 
// //             firstHeaderTextNodeIndex <= (headerChildren.length - 2)
// //             ? headerChildren[firstHeaderTextNodeIndex + 1]
// //             : undefined;
// //        
// //         if (afterFirstHeaderTextNode) {
// //             annotation.header.insertBefore(toggleButton, afterFirstHeaderTextNode);
// //         } else {
// //             annotation.header.appendChild(toggleButton);
// //         }
// //         if (headerChildren.length === 1) {
// //             annotation.header.appendChild(toggleButton);    
// //         }
// //     }
// //    
// //     findAnnotations().forEach(configureAnnotation);
// // })