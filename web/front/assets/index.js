(async () => {
    const createImage = (src, width, height) => {
        const image = document.createElement('img');
        image.setAttribute('src', src);
        image.setAttribute('width', width);
        image.setAttribute('height', height);
        return image;
    };

    const form = document.getElementsByTagName('form')[0];
    form.addEventListener('submit', async (e) => {
        e.preventDefault();

        const resultsContainer = document.getElementById('resultsContainer');
        const maxDominosContainer = document.getElementById('maxDominos');
        resultsContainer.innerHTML = '';
        maxDominosContainer.innerHTML = '';

        const formData = Object.fromEntries(new FormData(form));
        const requestBody = {
            rows: parseInt(formData.rows),
            columns: parseInt(formData.columns),
            bomb1: {
                row: parseInt(formData.bomb1Row),
                column: parseInt(formData.bomb1Column)
            },
            bomb2: {
                row: parseInt(formData.bomb2Row),
                column: parseInt(formData.bomb2Column)
            }
        };
        const algorithm = formData.algorithm;
        const resultType = formData.resultType;

        const headers = new Headers();
        headers.append('Content-Type', 'application/json');
        const raw = JSON.stringify(requestBody);
        const requestOptions = {
            method: 'POST',
            headers,
            body: raw
        };

        const response = await fetch(
            `http://localhost:2005/${algorithm}/${resultType}`,
            requestOptions
        );
        const responseBody = await response.json();
        if (response.status >= 400) {
            alert(responseBody.error);
            return;
        }
        const { maxDominos, results } = responseBody;
        const IMAGE_WIDTH = 50;
        const IMAGE_HEIGHT = 50;

        maxDominosContainer.innerText = `The maximum number of dominos that can be placed is ${maxDominos}`;

        results.forEach((result) => {
            const table = document.createElement('table');
            resultsContainer.appendChild(table);

            // Creating an invisible row fixes some glitches
            const columns = result[0].length;
            const invisibleRow = document.createElement('tr');
            for (let i = 0; i < columns; ++i) {
                const cell = document.createElement('td');
                cell.classList.add('invisible');
                invisibleRow.appendChild(cell);
            }
            table.appendChild(invisibleRow);

            result.forEach((row) => {
                const tableRow = document.createElement('tr');
                table.appendChild(tableRow);
                row.filter((cell) => cell !== 'd' && cell !== 'r').forEach((cell) => {
                    const tableCell = document.createElement('td');
                    if (cell === 'u') {
                        tableCell.setAttribute('rowspan', 2);
                        tableCell.appendChild(
                            createImage(
                                '/assets/domino-vertical.svg',
                                IMAGE_WIDTH,
                                IMAGE_HEIGHT * 2
                            )
                        );
                    }
                    if (cell === 'l') {
                        tableCell.setAttribute('colspan', 2);
                        tableCell.appendChild(
                            createImage(
                                '/assets/domino-horizontal.svg',
                                IMAGE_WIDTH * 2,
                                IMAGE_HEIGHT
                            )
                        );
                    }
                    if (cell === 'b') {
                        tableCell.appendChild(
                            createImage('/assets/bomb.svg', IMAGE_WIDTH, IMAGE_HEIGHT)
                        );
                    }
                    tableRow.appendChild(tableCell);
                });
            });

            resultsContainer.appendChild(document.createElement('hr'));
        });
    });
})();
