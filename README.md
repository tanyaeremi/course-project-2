# course-project-2

Данный код помогает воспроизвести анализ, изложенный в курсовой работе "Исследование навыков фонематического восприятия у детей-дошкольников 5-6 лет".
Весь код был написан в среде R, помимо нее для успешного воспроизведения также понадобится загрузить excel-файлы с данными, на основе которых выполнялся весь анализ. 
Перед началом работы необходимо установить все пакеты с функциями, которые были использованы в анализе. 
После установки пакетов следует установить ту директорию, в которой будет хранится файл с кодом. Удобнее будет хранить в той же директории и файлы с данными. 
Установить директорию можно с помощью команды setwd().
Далее для воспроизведения описательной статистики следует загрузить датасет с полными результатами по всей выборке. 
Название файла: 2021_ReadingPredictors_summary_data.xlsx
Сделать это можно через вкладку Environment: Environment -> Import Dataset -> From Excel -> путь до датасета, который нужно загрузить -> Update -> Import
После этого необходимо прочитать импортированный датасет через команду read_excel().
Для продолжения анализа необходимо перевести данные в правильный формат.
Далее происходит рассчет среднего значения и стандартного отклонения для каждого фонологического субтеста в зависимости от пола участника.
Оценка нормальности распределения результатов производится с помощью функции hist() и теста shapiro.test().
Оценка статистичекской значимости определяется с помощью теста Манна-Уитни для двух независимых выборок: wilcox.test().
Построение боксплотов по каждому субтесту происходит с помощью команды boxplot().
Далее проводится анализ по тем же параметрам, с применением тех же функций для усеченной выборки с участниками из группы наследственного риска.
Датасет, необходимый для повторения этой части анализа: ParentsDiagnosis.xlsx
После воспроизведения части анализа усеченной выборки реализуется корреляционный анализ.
Коэффициент и значимость корреляции рассчитываются с помощью функции cor.test(), с применением метода Спирмена (так как распределение результатов ненормальное).
После рассчета каждой корреляции строится график с распределением результатов по возрасту (каждая возрастная категория отделяется цветом).
После корреляционного анализа происходит рассчет средней скорости реакции и стандартного отклонения для каждого субтеста в зависимости от пола участников.
Для этого рассчета снова понадобится датасет с результатами по общей выборке.
Далее идет детальный анализ по пробам каждого субтеста.
Для подсчета правильных и неправильных ответов в зависимости от типа стимулов в субтесте "Дискриминация фонем" необходимо прочитать датасет PhonDiscr_results.xlsx
Для подсчета правильных и неправильных ответов в зависимости от типа стимулов в субтесте "Лексическое решение" необходимо прочитать датасет LexDec_results.xlsx
Для подсчета правильных и неправильных ответов в зависимости от типа стимулов в субтесте "Наличие звука в слове" необходимо прочитать датасет Dyslexia8_Sound_In_Word_result.xlsx
Все графики строятся с помощью функций ggplot().
На этом воспроизведение анализа, описанного в данной курсовой работе, заканчивается.
