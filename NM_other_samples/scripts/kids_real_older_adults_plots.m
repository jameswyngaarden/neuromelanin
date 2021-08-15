%% Script

clear;
maindir = '/Users/jameswyngaarden/Desktop';

%% plotting NM values

% organize data
data = readmatrix(fullfile(maindir,'Social Doors Kids NM analysis tracker.xlsx'));
kids = data(1:8,:);
real_adults = data(9:14,:);
older_adults = data(15:20,:);

% kids bar graph NM full
x = 1:8;
figure1 = figure('Name','Kids NM full');
axes1 = axes('Parent',figure1);
hold(axes1,'on');
bar(x(1,1), kids(1,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,2), kids(2,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,3), kids(3,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,4), kids(4,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,5), kids(5,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,6), kids(6,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,7), kids(7,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,8), kids(8,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
xlabel('sub ID');
title('Kids NM full');
ylabel('Avg. signal NM full');
set(axes1,'XTick',[1 2 3 4 5 6 7 8],'XTickLabel',...
    {'s011', 's013', 's015', 's024', 's036', 's5051', 's5058', 's5085'});

% kids bar graph NM vstri
x = 1:8;
figure1 = figure('Name','Kids NM vstri');
axes1 = axes('Parent',figure1);
hold(axes1,'on');
bar(x(1,1), kids(1,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,2), kids(2,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,3), kids(3,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,4), kids(4,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,5), kids(5,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,6), kids(6,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,7), kids(7,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,8), kids(8,3),'FaceColor',[ .2 .6 .5]);
xlabel('sub ID');
title('Kids NM vstri');
ylabel('Avg. signal NM vstri');
set(axes1,'XTick',[1 2 3 4 5 6 7 8],'XTickLabel',...
    {'s011', 's013', 's015', 's024', 's036', 's5051', 's5058', 's5085'});

% real adults graph NM full
x = 1:6;
figure1 = figure('Name','Real Adults NM full');
axes1 = axes('Parent',figure1);
hold(axes1,'on');
bar(x(1,1), real_adults(1,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,2), real_adults(2,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,3), real_adults(3,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,4), real_adults(4,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,5), real_adults(5,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,6), real_adults(6,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
xlabel('sub ID');
title('Real Adults NM full');
ylabel('Avg. signal NM full');
set(axes1,'XTick',[1 2 3 4 5 6 7 8],'XTickLabel',...
    {'s523', 's571', 's578', 's606', 's707', 's730'});

% real adults graph NM vstri
x = 1:6;
figure1 = figure('Name','Real Adults NM vstri');
axes1 = axes('Parent',figure1);
hold(axes1,'on');
bar(x(1,1), real_adults(1,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,2), real_adults(2,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,3), real_adults(3,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,4), real_adults(4,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,5), real_adults(5,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,6), real_adults(6,3),'FaceColor',[ .2 .6 .5]);
xlabel('sub ID');
title('Real Adults NM vstri');
ylabel('Avg. signal NM vstri');
set(axes1,'XTick',[1 2 3 4 5 6 7 8],'XTickLabel',...
    {'s523', 's571', 's578', 's606', 's707', 's730'});

% older adults graph NM full
x = 1:6;
figure1 = figure('Name','Older Adults NM full');
axes1 = axes('Parent',figure1);
hold(axes1,'on');
bar(x(1,1), older_adults(1,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,2), older_adults(2,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,3), older_adults(3,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,4), older_adults(4,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,5), older_adults(5,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
bar(x(1,6), older_adults(6,2),'FaceColor',[ 0.5843 0.8157 0.9882]);
xlabel('sub ID');
title('Older Adults NM full');
ylabel('Avg. signal NM full');
set(axes1,'XTick',[1 2 3 4 5 6 7 8],'XTickLabel',...
    {'s001', 's002', 's003', 's004', 's005', 's006'});

% older adults graph NM vstri
x = 1:6;
figure1 = figure('Name','Older Adults NM vstri');
axes1 = axes('Parent',figure1);
hold(axes1,'on');
bar(x(1,1), older_adults(1,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,2), older_adults(2,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,3), older_adults(3,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,4), older_adults(4,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,5), older_adults(5,3),'FaceColor',[ .2 .6 .5]);
bar(x(1,6), older_adults(6,3),'FaceColor',[ .2 .6 .5]);
xlabel('sub ID');
title('Older Adults NM vstri');
ylabel('Avg. signal NM vstri');
set(axes1,'XTick',[1 2 3 4 5 6 7 8],'XTickLabel',...
    {'s001', 's002', 's003', 's004', 's005', 's006'});




