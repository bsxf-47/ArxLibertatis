/*
 * Copyright 2016 Arx Libertatis Team (see the AUTHORS file)
 *
 * This file is part of Arx Libertatis.
 *
 * Arx Libertatis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Arx Libertatis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Arx Libertatis.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef ARX_TOOLS_VIEWER_UI_ARXVIEWER_H
#define ARX_TOOLS_VIEWER_UI_ARXVIEWER_H

#include <QGraphicsView>
#include <QMainWindow>

class ArxRenderWidget;
class AssetsModel;
class AnimationLayersModel;
class LightsModel;

namespace Ui {
class ArxViewerClass;
}

class ArxViewer : public QMainWindow {
	Q_OBJECT
	
public:
	ArxViewer(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~ArxViewer();
	
private slots:
	void on_assetsTree_doubleClicked(const QModelIndex &index);
	void on_pushButton_clicked();
	void on_actionShow_Face_Normals_toggled(bool arg1);
	void on_actionShow_Vertex_Normals_toggled(bool arg1);
	void on_pushButton_2_toggled(bool checked);
	void on_horizontalSlider_2_valueChanged(int value);
	
private:
	Ui::ArxViewerClass * ui;
	
	ArxRenderWidget *      m_renderWidget;
	AssetsModel *          m_assetsModel;
	AnimationLayersModel * m_animationLayersModel;
	LightsModel *          m_lightsModel;
};

#endif // ARX_TOOLS_VIEWER_UI_ARXVIEWER_H
