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

        const headers = new Headers();
        headers.append('Content-Type', 'application/json');
        const raw = JSON.stringify(requestBody);
        const requestOptions = {
            method: 'POST',
            headers,
            body: raw
        };
        const response = await (
            await fetch('http://localhost:2005/uninformed', requestOptions)
        ).json();

        const results = response.results;
        const resultsContainer = document.getElementById('resultsContainer');

        const IMAGE_WIDTH = 50;
        const IMAGE_HEIGHT = 50;

        results.forEach((result) => {
            const table = document.createElement('table');
            resultsContainer.appendChild(table);

            result.forEach((row) => {
                const tableRow = document.createElement('tr');
                table.appendChild(tableRow);
                row.filter((cell) => cell !== 'd' && cell !== 'r').forEach((cell) => {
                    const tableCell = document.createElement('td');
                    if (cell === 'u') {
                        tableCell.setAttribute('rowspan', 2);
                        tableCell.appendChild(
                            createImage('/assets/domino-vertical.svg', IMAGE_WIDTH, IMAGE_HEIGHT * 2)
                        );
                    }
                    if (cell === 'l') {
                        tableCell.setAttribute('colspan', 2);
                        tableCell.appendChild(
                            createImage('/assets/domino-horizontal.svg', IMAGE_WIDTH * 2, IMAGE_HEIGHT)
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

            resultsContainer.appendChild(document.createElement('br'));
            resultsContainer.appendChild(document.createElement('br'));
        });
    });
})();
